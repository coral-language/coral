//! Comment storage and management for documentation and IDE support.
//!
//! Comments are stored separately from the AST to maintain performance and
//! avoid modifying every AST node. This follows Python's approach where
//! comments are preserved for tooling but don't affect the semantic AST.

use smallvec::SmallVec;
use text_size::TextRange;

/// A single comment in the source code.
#[derive(Debug, Clone)]
pub struct Comment {
    /// The comment text without the '#' prefix, trimmed of whitespace.
    pub text: String,
    /// The span of the comment in the source text (including '#').
    pub span: TextRange,
    /// The kind of comment (leading/trailing).
    pub kind: CommentKind,
}

/// The kind of comment - affects how it's attached to AST nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    /// Comment on its own line or at the start of a line.
    Line,
    /// Comment at the end of a line after code.
    Trailing,
}

/// Storage for all comments in a source file.
/// Uses SmallVec to avoid heap allocation for typical files (< 32 comments).
#[derive(Debug, Clone)]
pub struct CommentMap {
    comments: SmallVec<[Comment; 32]>,
}

impl CommentMap {
    /// Create a new empty comment map.
    pub fn new() -> Self {
        CommentMap {
            comments: SmallVec::new(),
        }
    }

    /// Add a comment to the map.
    pub fn add_comment(&mut self, text: String, span: TextRange, kind: CommentKind) {
        self.comments.push(Comment { text, span, kind });
    }

    /// Get all comments in the map.
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    /// Get comments that start before or at the given position.
    /// Useful for attaching comments to AST nodes.
    pub fn comments_before(&self, pos: TextRange) -> &[Comment] {
        let idx = self
            .comments
            .partition_point(|c| c.span.end() <= pos.start());
        &self.comments[..idx]
    }

    /// Check if the map is empty.
    pub fn is_empty(&self) -> bool {
        self.comments.is_empty()
    }

    /// Get the number of comments stored.
    pub fn len(&self) -> usize {
        self.comments.len()
    }
}

impl Default for CommentMap {
    fn default() -> Self {
        Self::new()
    }
}
