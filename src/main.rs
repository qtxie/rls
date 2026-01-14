use anyhow::Result;
use crossbeam_channel::Sender;
use log::info;
use lsp_server::{Connection, ExtractError, Message, Request, Response};
use lsp_types::{GotoDefinitionResponse, *};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tree_sitter::{Parser, Tree};

mod parser;

#[derive(Debug, Deserialize, Serialize)]
struct InitializeParams {}

struct Document {
    content: String,
    tree: Option<Tree>,
    parser: Parser,
    diagnostics: Vec<lsp_types::Diagnostic>,
}

struct RedLanguageServer {
    documents: HashMap<String, Document>,
    sender: Sender<lsp_server::Message>,
}

impl RedLanguageServer {
    fn new(sender: Sender<lsp_server::Message>) -> Self {
        Self {
            documents: HashMap::new(),
            sender,
        }
    }

    fn handle_initialize(&mut self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )), // Changed to INCREMENTAL for better performance
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["/".to_string()]),
                    all_commit_characters: None,
                    completion_item: None,
                    work_done_progress_options: Default::default(),
                }),
                ..Default::default()
            },
            server_info: None,
        })
    }

    fn handle_text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        let mut parser = Parser::new();
        // Set the parser language to Red
        let lang = tree_sitter_red::LANGUAGE;
        match parser.set_language(&lang.into()) {
            Ok(()) => {
                eprintln!("Successfully set language for parser");
            }
            Err(e) => {
                eprintln!("Failed to set language for parser: {:?}", e);
                // Return early if we can't set the language
                return;
            }
        }

        // Parse the full document
        let tree = parser.parse(&params.text_document.text, None);
        if tree.is_none() {
            eprintln!("Failed to parse document");
        } else {
            // Check if the tree has errors
            if let Some(ref t) = tree {
                if t.root_node().has_error() {
                    eprintln!("Parse tree contains errors");
                }
            }
        }

        // Compute diagnostics for the parsed tree
        let diagnostics = if let Some(ref t) = tree {
            info!("{}", t.root_node().to_sexp());
            parser::get_diagnostics(&params.text_document.text, t)
        } else {
            info!("empty tree");
            vec![]
        };

        // Send diagnostics to the client before storing the document
        self.publish_diagnostics(&params.text_document.uri, diagnostics.clone());

        let document = Document {
            content: params.text_document.text,
            tree,
            parser,
            diagnostics,
        };

        self.documents
            .insert(params.text_document.uri.to_string(), document);
    }

    fn handle_text_document_did_change(&mut self, params: DidChangeTextDocumentParams) {
        if let Some(document) = self
            .documents
            .get_mut(&params.text_document.uri.to_string())
        {
            for change in params.content_changes {
                if let Some(range) = change.range {
                    // Store the old content length before the change
                    let _old_length = document.content.len();

                    // Store the original content before the change
                    let old_content = document.content.clone();

                    // Apply the change to the content
                    document.content = apply_content_change(&old_content, &change.text, range);

                    // Update the tree incrementally if it exists
                    if let Some(ref mut tree) = document.tree {
                        // Calculate the byte positions and points for the edit
                        let start_byte = position_to_offset(&old_content, range.start);
                        let old_end_byte = position_to_offset(&old_content, range.end);
                        let new_end_byte = start_byte + change.text.len();

                        // Calculate the start and end points
                        let start_point = tree_sitter::Point {
                            row: range.start.line as usize,
                            column: range.start.character as usize,
                        };

                        // Calculate the old end point based on the original range
                        let old_end_point = tree_sitter::Point {
                            row: range.end.line as usize,
                            column: range.end.character as usize,
                        };

                        // Calculate the new end point based on the inserted text
                        let new_end_position = calculate_new_end_position(&document.content, range, &change.text);

                        // Tell the parser about the change
                        tree.edit(&tree_sitter::InputEdit {
                            start_byte,
                            old_end_byte,
                            new_end_byte,
                            start_position: start_point,
                            old_end_position: old_end_point,
                            new_end_position,
                        });

                        // Re-parse the modified tree
                        document.tree = document.parser.parse(&document.content, Some(tree));
                        if document.tree.is_none() {
                            eprintln!("Failed to re-parse document after edit");
                            // Fallback to full reparse
                            document.tree = document.parser.parse(&document.content, None);
                        }
                    }
                } else {
                    // Full document replacement - reparse from scratch
                    document.content = change.text;
                    document.tree = document.parser.parse(&document.content, None);
                    if document.tree.is_none() {
                        eprintln!("Failed to re-parse document after full replacement");
                    }
                }
            }

            // Recompute diagnostics for the updated document
            let new_diagnostics = if let Some(ref tree) = document.tree {
                parser::get_diagnostics(&document.content, tree)
            } else {
                vec![]
            };

            document.diagnostics = new_diagnostics.clone();

            // Send updated diagnostics to the client
            self.publish_diagnostics(&params.text_document.uri, new_diagnostics);
        }
    }

    fn handle_goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Option<GotoDefinitionResponse> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(document) = self.documents.get(&uri.to_string()) {
            // Use the stored tree to find definitions
            if let Some(ref tree) = document.tree {
                if let Some(mut location) =
                    parser::find_definition_at_position_with_tree(&document.content, tree, position)
                {
                    // Update the URI to match the current document
                    location.uri = uri.clone();
                    return Some(GotoDefinitionResponse::Scalar(location));
                }
            }
        }

        None
    }

    fn handle_completion(&self, params: CompletionParams) -> Option<lsp_types::CompletionResponse> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(document) = self.documents.get(&uri.to_string()) {
            // Use the stored tree to get completions
            if let Some(ref tree) = document.tree {
                let completions = parser::get_completions(&document.content, tree, position);
                return Some(lsp_types::CompletionResponse::Array(completions));
            }
        }

        Some(lsp_types::CompletionResponse::Array(Vec::new()))
    }

    fn publish_diagnostics(&self, uri: &lsp_types::Uri, diagnostics: Vec<lsp_types::Diagnostic>) {
        use serde_json::json;

        let params = lsp_types::PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics,
            version: None,
        };

        let notif = lsp_server::Notification {
            method: "textDocument/publishDiagnostics".to_string(),
            params: json!(params),
        };

        // Send the notification to the client
        let _ = self.sender.send(lsp_server::Message::Notification(notif));
    }
}

fn apply_content_change(content: &str, new_text: &str, range: Range) -> String {
    let start_line = range.start.line as usize;
    let start_char = range.start.character as usize;
    let end_line = range.end.line as usize;
    let end_char = range.end.character as usize;

    let content_lines: Vec<&str> = content.lines().collect();

    if start_line == end_line {
        // Same line replacement
        let mut result = String::new();
        for (i, line) in content_lines.iter().enumerate() {
            if i == start_line {
                let chars: Vec<char> = line.chars().collect();
                let prefix: String = chars[..start_char].iter().collect();
                let suffix: String = chars[end_char..].iter().collect();
                result.push_str(&prefix);
                result.push_str(new_text);
                result.push_str(&suffix);
            } else {
                result.push_str(line);
            }

            if i < content_lines.len() - 1 {
                result.push('\n');
            }
        }
        result
    } else {
        // Multi-line replacement
        let mut result = String::new();
        for (i, line) in content_lines.iter().enumerate() {
            if i == start_line {
                let chars: Vec<char> = line.chars().collect();
                let prefix: String = chars[..start_char].iter().collect();
                result.push_str(&prefix);
                result.push_str(new_text);
            } else if i > start_line && i < end_line {
                // Skip these lines as they are replaced
                continue;
            } else if i == end_line {
                let chars: Vec<char> = line.chars().collect();
                let suffix: String = chars[end_char..].iter().collect();
                result.push_str(&suffix);
            } else {
                result.push_str(line);
            }

            if i < content_lines.len() - 1 && i != end_line {
                result.push('\n');
            }
        }
        result
    }
}

fn position_to_offset(text: &str, position: Position) -> usize {
    let mut current_line = 0;
    let mut current_char = 0;
    let mut offset = 0;

    for ch in text.chars() {
        if current_line == position.line as u32 {
            if current_char == position.character as u32 {
                return offset;
            }
            current_char += 1;
        }

        if ch == '\n' {
            current_line += 1;
            current_char = 0;
        }

        offset += ch.len_utf8();

        if current_line > position.line as u32 {
            break;
        }
    }

    offset
}

fn calculate_new_end_position(content: &str, range: Range, new_text: &str) -> tree_sitter::Point {
    // Calculate the end position after inserting new_text
    // We need to calculate the position based on the start position plus the new text
    let start_line = range.start.line as usize;

    // Count newlines in the new text to determine how many lines were added
    let newline_count = new_text.matches('\n').count();

    // Calculate the column position in the last line of the new text
    let last_line_start = new_text.rfind('\n').map(|i| i + 1).unwrap_or(0);
    let last_line_length = new_text[last_line_start..].len();

    if newline_count == 0 {
        // Same line - just add the character count to the start character
        tree_sitter::Point {
            row: start_line,
            column: range.start.character as usize + last_line_length,
        }
    } else {
        // Multiple lines - calculate the final position
        tree_sitter::Point {
            row: start_line + newline_count,
            column: last_line_length,
        }
    }
}

fn main() -> Result<()> {
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Debug)
        .init();

    info!("Starting Red language server");
    // Create the transport
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end
    let result = run_server(connection);

    // Propagate errors
    result?;
    io_threads.join()?;

    Ok(())
}

fn run_server(connection: Connection) -> Result<()> {
    let mut server = RedLanguageServer::new(connection.sender.clone());

    // Handle initialization
    let (id, params) = connection.initialize_start()?;
    let initialize_params = serde_json::from_value(params)?;
    let server_capabilities = server.handle_initialize(initialize_params)?;
    connection.initialize_finish(id, serde_json::to_value(server_capabilities)?)?;

    // Run the event loop
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                let req_result = handle_request(&mut server, req);

                if let Some(response) = req_result? {
                    connection.sender.send(Message::Response(response))?;
                }
            }
            Message::Response(_resp) => {
                // Responses are handled by the client
            }
            Message::Notification(notification) => {
                handle_notification(&mut server, notification)?;
            }
        }
    }

    Ok(())
}

fn handle_request(
    server: &mut RedLanguageServer,
    req: Request,
) -> std::result::Result<Option<Response>, ExtractError<Request>> {
    let id = req.id.clone();

    match req.method.as_str() {
        "textDocument/definition" => {
            match serde_json::from_value::<GotoDefinitionParams>(req.params) {
                Ok(params) => {
                    let result = server.handle_goto_definition(params);
                    Ok(Some(Response::new_ok(id, result)))
                }
                Err(_) => Ok(Some(Response::new_err(
                    id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    "Invalid params".to_string(),
                ))),
            }
        }
        "textDocument/completion" => match serde_json::from_value::<CompletionParams>(req.params) {
            Ok(params) => {
                let result = server.handle_completion(params);
                Ok(Some(Response::new_ok(id, result)))
            }
            Err(_) => Ok(Some(Response::new_err(
                id,
                lsp_server::ErrorCode::InvalidParams as i32,
                "Invalid params".to_string(),
            ))),
        },
        _ => Ok(None),
    }
}

fn handle_notification(
    server: &mut RedLanguageServer,
    notification: lsp_server::Notification,
) -> Result<()> {
    match notification.method.as_str() {
        "textDocument/didOpen" => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(notification.params)?;
            server.handle_text_document_did_open(params);
        }
        "textDocument/didChange" => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(notification.params)?;
            server.handle_text_document_did_change(params);
        }
        _ => {}
    }

    Ok(())
}
