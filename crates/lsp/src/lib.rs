//! # Coral Language Server Protocol (LSP)
//!
//! This crate provides Language Server Protocol implementation for the Coral programming language.
//! It enables IDE integration with features like syntax highlighting, error reporting,
//! code completion, and go-to-definition.
//!
//! ## Features
//!
//! - **Syntax Highlighting**: Token-based syntax highlighting
//! - **Error Diagnostics**: Real-time error reporting with source locations
//! - **Code Completion**: Intelligent code completion suggestions
//! - **Go-to-Definition**: Navigate to symbol definitions
//! - **Hover Information**: Show documentation and type information
//! - **Formatting**: Source code formatting and indentation
//!
//! ## Architecture
//!
//! The LSP implementation is organized into:
//! - `server`: Main LSP server implementation and request handling
//! - `diagnostics`: Error reporting and diagnostic generation
//!
//! ## Usage
//!
//! ```rust
//! use coral_lsp::server::LspServer;
//!
//! // Create and run the LSP server
//! let server = LspServer::new();
//! server.start();
//! ```
//!
//! ## IDE Integration
//!
//! The LSP server can be integrated with various IDEs and editors:
//! - Visual Studio Code
//! - Vim/Neovim
//! - Emacs
//! - Sublime Text
//! - IntelliJ IDEA

pub mod diagnostics;
pub mod server;
