//! Error context for rich diagnostics.

use text_size::TextRange;

/// Context information for an error.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ErrorContext {
    /// The source file name (if available)
    pub filename: Option<String>,
    /// The source code
    pub source: String,
    /// The error location
    pub span: TextRange,
    /// Additional context message
    pub message: Option<String>,
    /// Suggestion for fixing the error
    pub suggestion: Option<String>,
    /// Related spans (e.g., for "defined here" notes)
    pub related_spans: Vec<(TextRange, String)>,
}

impl ErrorContext {
    /// Create a new error context.
    pub fn new(source: String, span: TextRange) -> Self {
        ErrorContext {
            filename: None,
            source,
            span,
            message: None,
            suggestion: None,
            related_spans: Vec::new(),
        }
    }

    /// Set the filename.
    pub fn with_filename(mut self, filename: String) -> Self {
        self.filename = Some(filename);
        self
    }

    /// Add a context message.
    pub fn with_message(mut self, message: String) -> Self {
        self.message = Some(message);
        self
    }

    /// Add a suggestion for fixing the error.
    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestion = Some(suggestion);
        self
    }

    /// Add a related span with a note.
    pub fn add_related_span(&mut self, span: TextRange, note: String) {
        self.related_spans.push((span, note));
    }

    /// Get the line and column number for the error.
    pub fn line_and_column(&self) -> (usize, usize) {
        let start: usize = self.span.start().into();
        let line = self.source[..start].chars().filter(|&c| c == '\n').count() + 1;
        let line_start = self.source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let col = start - line_start + 1;
        (line, col)
    }

    /// Get the line containing the error.
    pub fn error_line(&self) -> &str {
        let start: usize = self.span.start().into();
        let end: usize = self.span.end().into();

        let line_start = self.source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let line_end = self.source[end..]
            .find('\n')
            .map(|i| end + i)
            .unwrap_or(self.source.len());

        &self.source[line_start..line_end]
    }

    /// Get context lines around the error.
    pub fn context_lines(&self, num_lines: usize) -> Vec<(usize, String)> {
        let start: usize = self.span.start().into();
        let lines: Vec<&str> = self.source.lines().collect();
        let error_line_num = self.source[..start].chars().filter(|&c| c == '\n').count();

        let start_line = error_line_num.saturating_sub(num_lines);
        let end_line = (error_line_num + num_lines + 1).min(lines.len());

        lines[start_line..end_line]
            .iter()
            .enumerate()
            .map(|(i, line)| (start_line + i + 1, line.to_string()))
            .collect()
    }
}
