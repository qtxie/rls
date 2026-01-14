use lsp_types::{Diagnostic, DiagnosticSeverity, Location, Position, Range};
use tree_sitter::{Node, Tree};
use url::Url;

// Helper function to get the Red language
pub fn get_red_language() -> tree_sitter::Language {
    // Use the LANGUAGE constant and convert it properly
    unsafe {
        std::mem::transmute::<tree_sitter_language::LanguageFn, tree_sitter::Language>(
            tree_sitter_red::LANGUAGE,
        )
    }
}

// New function that accepts an existing tree for efficiency
pub fn find_definition_at_position_with_tree(
    source_code: &str,
    tree: &Tree,
    position: Position,
) -> Option<Location> {
    // Convert LSP position to byte offset
    let byte_offset = position_to_offset(source_code, position);

    // Find the node at the given position
    let node = get_node_at_byte_offset(tree, byte_offset)?;

    // Check if the node represents a reference to a definition
    if is_reference_node(&node) {
        // Find the corresponding definition in the AST
        let definition_node = find_definition_node(source_code, tree, &node)?;

        // Convert the tree-sitter range to LSP range
        let range = ts_range_to_lsp_range(
            definition_node.start_byte(),
            definition_node.end_byte(),
            source_code,
        );

        // The URI will be set by the caller to match the current document
        // Using a placeholder URI here that will be replaced
        let uri = Url::parse("file:///placeholder").ok()?;

        Some(Location { uri, range })
    } else {
        None
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

fn get_node_at_byte_offset<'a>(tree: &'a Tree, byte_offset: usize) -> Option<Node<'a>> {
    let mut cursor = tree.walk();
    let mut node = tree.root_node();

    loop {
        for child in node.children(&mut cursor) {
            if child.start_byte() <= byte_offset && byte_offset < child.end_byte() {
                node = child;
                continue;
            }
        }
        break;
    }

    Some(node)
}

fn is_reference_node(node: &Node) -> bool {
    // In Red language, we typically want to find references to words/functions
    // Check for specific node types that represent references to definitions
    // This depends on the tree-sitter-red grammar
    node.kind() == "word"
}

fn find_definition_node<'a>(
    source_code: &str,
    tree: &'a Tree,
    reference_node: &Node<'a>,
) -> Option<Node<'a>> {
    // Get the text of the referenced word
    let reference_text = get_node_text(source_code, reference_node)?;

    // Walk the tree to find a definition with the same name
    let root = tree.root_node();

    find_definition_by_name(source_code, &root, &reference_text)
}

fn find_definition_by_name<'a>(source_code: &str, node: &Node<'a>, name: &str) -> Option<Node<'a>> {
    // Check if this node is a definition with the requested name
    if is_definition_node(node) && node_matches_name(source_code, node, name) {
        return Some(*node);
    }

    // Recursively check children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(def_node) = find_definition_by_name(source_code, &child, name) {
            return Some(def_node);
        }
    }

    None
}

fn is_definition_node(node: &Node) -> bool {
    // In Red, definitions could be function definitions, variable assignments, etc.
    // Common patterns in Red for definitions:
    // - Function definitions: `func-name: func [...] [...]`
    // - Variable assignments: `var-name: value`
    // This depends on the tree-sitter-red grammar
    node.kind() == "assignment" || node.kind() == "function_definition"
}

fn node_matches_name(source_code: &str, node: &Node, name: &str) -> bool {
    // Extract the identifier/name from the definition node
    // This depends on the exact structure of the tree-sitter-red grammar

    // For assignment nodes, the left side is usually the name being defined
    if node.kind() == "assignment" {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "word" || child.kind() == "identifier" {
                if get_node_text(source_code, &child).map_or(false, |text| text == name) {
                    return true;
                }
            }
        }
    }

    // For function definitions, the name is typically the first child
    if node.kind() == "function_definition" {
        if let Some(first_child) = node.child(0) {
            if get_node_text(source_code, &first_child).map_or(false, |text| text == name) {
                return true;
            }
        }
    }

    false
}

fn get_node_text<'a>(source_code: &'a str, node: &Node) -> Option<&'a str> {
    Some(&source_code[node.start_byte()..node.end_byte()])
}

fn ts_range_to_lsp_range(start_byte: usize, end_byte: usize, source: &str) -> Range {
    let start_pos = byte_offset_to_position(source, start_byte);
    let end_pos = byte_offset_to_position(source, end_byte);

    Range {
        start: start_pos,
        end: end_pos,
    }
}

fn byte_offset_to_position(source_code: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut character = 0;
    let mut current_offset = 0;

    for ch in source_code.chars() {
        if current_offset >= offset {
            break;
        }

        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }

        current_offset += ch.len_utf8();
    }

    Position {
        line: line as u32,
        character: character as u32,
    }
}

// Function to collect diagnostics from the parsed tree
pub fn get_diagnostics(source_code: &str, tree: &Tree) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Walk the tree to find any error nodes or invalid tokens
    let mut cursor = tree.walk();
    collect_error_nodes(source_code, &mut cursor, &mut diagnostics);

    diagnostics
}

// Function to provide code completion suggestions
pub fn get_completions(
    source_code: &str,
    tree: &Tree,
    position: Position,
) -> Vec<lsp_types::CompletionItem> {
    let mut completions = Vec::new();

    // Get all identifiers and function names in the document for completion
    let mut cursor = tree.walk();
    collect_identifiers(source_code, &mut cursor, &mut completions, position);

    // Add some common Red keywords as completions
    add_common_keywords(&mut completions);

    completions
}

// Recursive function to collect identifiers from the tree
fn collect_identifiers(
    source_code: &str,
    cursor: &mut tree_sitter::TreeCursor,
    completions: &mut Vec<lsp_types::CompletionItem>,
    position: Position,
) {
    loop {
        let node = cursor.node();

        // Check if this node is an identifier or function name
        if node.kind() == "word" || node.kind() == "identifier" {
            let text = get_node_text(source_code, &node).unwrap_or("");

            // Only add if it's not already in the completions list
            if !completions.iter().any(|item| item.label == text) {
                let completion_item = lsp_types::CompletionItem {
                    label: text.to_string(),
                    kind: Some(lsp_types::CompletionItemKind::VARIABLE),
                    detail: Some(format!("Variable: {}", text)),
                    ..Default::default()
                };

                completions.push(completion_item);
            }
        } else if node.kind() == "function_definition" {
            // For function definitions, extract the function name
            let mut child_cursor = node.walk();
            for child in node.children(&mut child_cursor) {
                if child.kind() == "word" || child.kind() == "identifier" {
                    let text = get_node_text(source_code, &child).unwrap_or("");

                    if !completions.iter().any(|item| item.label == text) {
                        let completion_item = lsp_types::CompletionItem {
                            label: text.to_string(),
                            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                            detail: Some(format!("Function: {}", text)),
                            ..Default::default()
                        };

                        completions.push(completion_item);
                    }
                    break; // Take the first identifier as the function name
                }
            }
        }

        // Recurse into children
        if cursor.goto_first_child() {
            collect_identifiers(source_code, cursor, completions, position);
            cursor.goto_parent();
        }

        if !cursor.goto_next_sibling() {
            break;
        }
    }
}

// Function to add common Red keywords to completions
fn add_common_keywords(completions: &mut Vec<lsp_types::CompletionItem>) {
    let keywords = [
        "func",
        "function",
        "action",
        "routine",
        "op!",
        "native!",
        "if",
        "unless",
        "either",
        "case",
        "switch",
        "while",
        "until",
        "loop",
        "repeat",
        "for",
        "forall",
        "foreach",
        "forskip",
        "return",
        "exit",
        "break",
        "continue",
        "true",
        "false",
        "none",
        "self",
        "this",
        "do",
        "bind",
        "use",
        "has",
        "with",
        "set",
        "get",
        "let",
        "any",
        "all",
        "collect",
        "keep",
        "print",
        "prin",
        "probe",
        "form",
        "mold",
        "reduce",
        "compose",
        "construct",
        "length?",
        "count",
        "find",
        "select",
        "pick",
        "put",
        "remove",
        "insert",
        "append",
        "make",
        "to",
        "as",
        "copy",
        "clone",
        "clear",
        "head",
        "tail",
        "next",
        "back",
        "first",
        "second",
        "third",
        "fourth",
        "fifth",
        "last",
        "take",
        "skip",
        "at",
        "reverse",
        "sort",
        "unique",
        "random",
        "trim",
        "pad",
        "rejoin",
        "join",
        "split",
        "lowercase",
        "uppercase",
        "titlecase",
        "entab",
        "detab",
        "size",
        "offset?",
        "abs",
        "add",
        "divide",
        "multiply",
        "negate",
        "power",
        "remainder",
        "round",
        "subtract",
        "and~",
        "complement",
        "or~",
        "xor~",
        "even?",
        "odd?",
        "charset",
        "cksum",
        "debase",
        "enbase",
        "decode",
        "encode",
        "ip#",
        "mac",
        "now",
        "date",
        "time",
        "to-local-date",
        "extract",
        "difference",
        "exclude",
        "intersect",
        "union",
        "unique",
        "change-dir",
        "clean-path",
        "dir?",
        "exists?",
        "list-dir",
        "size?",
        "modify",
        "rename",
        "save",
        "load",
        "read",
        "write",
        "delete",
        "open",
        "close",
        "input",
        "output",
        "err",
        "open-events",
        "wait",
        "wake-up",
        "throw",
        "catch",
        "try",
        "attempt",
        "error?",
        "cause",
        "path-thru",
        "do-codec",
        "register-codec",
        "all-tags",
        "tag?",
        "binary?",
        "block?",
        "char?",
        "datatype?",
        "file?",
        "function?",
        "get-word?",
        "integer?",
        "issue?",
        "lit-word?",
        "logic?",
        "paren?",
        "path?",
        "refinement?",
        "series?",
        "string?",
        "unset?",
        "url?",
        "word?",
        "any-list?",
        "any-string?",
        "number?",
        "scalar?",
        "any-type?",
        "types-of",
        "what-dir",
        "change-dir",
        "dirize",
        "recycle",
        "stats",
        "license",
        "about",
        "help",
        "usage",
        "source",
        "what",
        "system",
        "system/standard",
        "system/catalog",
        "system/schemes",
        "system/contexts",
        "system/options",
        "system/state",
        "system/words",
        "system/platform",
        "system/version",
        "system/build",
    ];

    for keyword in keywords.iter() {
        if !completions.iter().any(|item| item.label == *keyword) {
            let completion_item = lsp_types::CompletionItem {
                label: keyword.to_string(),
                kind: Some(lsp_types::CompletionItemKind::KEYWORD),
                detail: Some(format!("Keyword: {}", keyword)),
                ..Default::default()
            };

            completions.push(completion_item);
        }
    }
}

// Recursive function to collect error nodes from the tree
fn collect_error_nodes(
    source_code: &str,
    cursor: &mut tree_sitter::TreeCursor,
    diagnostics: &mut Vec<Diagnostic>,
) {
    loop {
        let node = cursor.node();

        // Check if this node is an error or has an error child
        if node.is_error() || node.is_missing() {
            let range = ts_range_to_lsp_range(node.start_byte(), node.end_byte(), source_code);

            let message = if node.is_missing() {
                format!("Missing {}", node.kind())
            } else {
                format!(
                    "Syntax error: unexpected token '{}'",
                    get_node_text(source_code, &node).unwrap_or("<unknown>")
                )
            };

            let diagnostic = Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("red-lsp".to_string()),
                message,
                related_information: None,
                tags: None,
                data: None,
            };

            diagnostics.push(diagnostic);
        } else if node.kind() == "invalid_token" {
            // Specifically look for invalid_token nodes
            let range = ts_range_to_lsp_range(node.start_byte(), node.end_byte(), source_code);

            let diagnostic = Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("red-lsp".to_string()),
                message: format!(
                    "Invalid token: {}",
                    get_node_text(source_code, &node).unwrap_or("<unknown>")
                ),
                related_information: None,
                tags: None,
                data: None,
            };

            diagnostics.push(diagnostic);
        }

        // Recurse into children
        if cursor.goto_first_child() {
            collect_error_nodes(source_code, cursor, diagnostics);
            cursor.goto_parent();
        }

        if !cursor.goto_next_sibling() {
            break;
        }
    }
}
