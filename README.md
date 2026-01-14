# Red-LSP - Red Language Server

Red-LSP is a language server for the Red programming language, implementing the Language Server Protocol (LSP). It provides IDE-like features for Red developers.

## Features

- **Go to Definition**: Navigate to the definition of functions, variables, and other symbols in your Red code
- Syntax highlighting support through LSP
- Parsing of Red language constructs using tree-sitter-red

## Installation

```bash
# Clone the repository
git clone <repository-url>
cd red-lsp

# Build the project
cargo build --release
```

## Usage

The language server can be integrated with any editor that supports LSP. For example, with VSCode, you would configure it to use this executable as the Red language server.

## Architecture

The server uses:
- `tree-sitter-red` (from github.com/red/tree-sitter-red) for parsing Red code
- `lsp-server` and `lsp-types` for LSP implementation
- Custom logic for semantic analysis and feature implementation

## Current Implementation

Currently, the "Go to Definition" feature works by:
1. Parsing the entire file when opened using tree-sitter-red to create an AST
2. Storing the parsed AST in memory for efficient access
3. Incrementally updating the AST when document changes occur
4. Identifying the symbol under the cursor by traversing the stored AST
5. Finding the corresponding definition in the AST
6. Returning the location of the definition

The language server also includes diagnostic functionality that:
1. Detects invalid tokens and syntax errors in the parsed tree
2. Reports diagnostics to the client when files are opened or modified
3. Highlights errors in the user's editor

Additionally, the language server provides code completion functionality that:
1. Suggests identifiers and function names found in the current document
2. Provides common Red language keywords as completion options
3. Triggers completion on '.' and ':' characters
4. Offers context-aware suggestions based on the parsed AST

## Future Enhancements

- Cross-file symbol resolution
- Hover information
- Find all references
- Rename symbol
