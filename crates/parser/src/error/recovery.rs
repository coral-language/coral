//! Error recovery strategies for resilient parsing.

use crate::lexer::TokenKind;
use text_size::TextRange;

/// Error recovery strategy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RecoveryStrategy {
    /// Skip to next statement (find synchronization point)
    SkipToNextStatement,
    /// Skip to next line
    SkipToNextLine,
    /// Skip to closing delimiter
    SkipToClosing,
    /// Insert missing token
    InsertToken,
    /// Delete unexpected token
    DeleteToken,
    /// No recovery (fatal error)
    None,
}

/// Synchronization points for panic mode recovery.
/// These are tokens that typically mark the start of a new statement,
/// allowing the parser to resynchronize after an error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncPoint {
    /// Statement-level keywords
    Statement,
    /// Expression boundaries
    Expression,
    /// Block boundaries (dedent, opening braces)
    Block,
    /// End of file
    Eof,
}

impl SyncPoint {
    /// Check if a token is a synchronization point of this type.
    pub fn is_sync_token(&self, token: &TokenKind) -> bool {
        match self {
            SyncPoint::Statement => Self::is_statement_keyword(token),
            SyncPoint::Expression => {
                Self::is_statement_keyword(token) || matches!(token, TokenKind::Newline)
            }
            SyncPoint::Block => {
                Self::is_statement_keyword(token)
                    || matches!(token, TokenKind::Dedent | TokenKind::Newline)
            }
            SyncPoint::Eof => matches!(token, TokenKind::Eof),
        }
    }

    /// Check if a token is a statement keyword (sync point).
    fn is_statement_keyword(token: &TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Def
                | TokenKind::Class
                | TokenKind::If
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Try
                | TokenKind::With
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Pass
                | TokenKind::Raise
                | TokenKind::Import
                | TokenKind::From
                | TokenKind::Global
                | TokenKind::Nonlocal
                | TokenKind::Assert
                | TokenKind::Del
                | TokenKind::Async
                | TokenKind::Match
                | TokenKind::Type
                | TokenKind::At // Decorator
                | TokenKind::Newline
                | TokenKind::Dedent
        )
    }
}

/// Configuration for error recovery behavior.
#[derive(Debug, Clone)]
pub struct RecoveryConfig {
    /// Maximum number of errors to recover from before giving up
    pub max_recoveries: usize,
    /// Maximum number of tokens to skip during recovery
    pub max_skip_tokens: usize,
    /// Default synchronization point type
    pub default_sync_point: SyncPoint,
}

impl Default for RecoveryConfig {
    fn default() -> Self {
        RecoveryConfig {
            max_recoveries: 100,
            max_skip_tokens: 50,
            default_sync_point: SyncPoint::Statement,
        }
    }
}

/// Recovery action taken during parsing.
#[derive(Debug, Clone)]
pub struct RecoveryAction {
    /// The strategy used
    pub strategy: RecoveryStrategy,
    /// Location where recovery was applied
    pub span: TextRange,
    /// Description of the recovery action
    pub description: String,
    /// Number of tokens skipped (if applicable)
    pub tokens_skipped: usize,
}

impl RecoveryAction {
    /// Create a new recovery action.
    pub fn new(strategy: RecoveryStrategy, span: TextRange, description: String) -> Self {
        RecoveryAction {
            strategy,
            span,
            description,
            tokens_skipped: 0,
        }
    }

    /// Create a recovery action with token skip count.
    pub fn with_skip_count(
        strategy: RecoveryStrategy,
        span: TextRange,
        description: String,
        tokens_skipped: usize,
    ) -> Self {
        RecoveryAction {
            strategy,
            span,
            description,
            tokens_skipped,
        }
    }
}

/// Error recovery manager.
pub struct RecoveryManager {
    actions: Vec<RecoveryAction>,
    config: RecoveryConfig,
}

impl RecoveryManager {
    /// Create a new recovery manager with default configuration.
    pub fn new() -> Self {
        Self::with_config(RecoveryConfig::default())
    }

    /// Create a recovery manager with custom configuration.
    pub fn with_config(config: RecoveryConfig) -> Self {
        RecoveryManager {
            actions: Vec::new(),
            config,
        }
    }

    /// Record a recovery action.
    pub fn record(&mut self, action: RecoveryAction) {
        self.actions.push(action);
    }

    /// Check if recovery limit has been reached.
    pub fn limit_reached(&self) -> bool {
        self.actions.len() >= self.config.max_recoveries
    }

    /// Get the maximum number of tokens that can be skipped.
    pub fn max_skip_tokens(&self) -> usize {
        self.config.max_skip_tokens
    }

    /// Get the default synchronization point type.
    pub fn default_sync_point(&self) -> SyncPoint {
        self.config.default_sync_point
    }

    /// Get all recovery actions.
    pub fn actions(&self) -> &[RecoveryAction] {
        &self.actions
    }

    /// Clear all recovery actions.
    pub fn clear(&mut self) {
        self.actions.clear();
    }

    /// Get statistics about recovery actions.
    pub fn stats(&self) -> RecoveryStats {
        let total_actions = self.actions.len();
        let total_tokens_skipped = self.actions.iter().map(|a| a.tokens_skipped).sum();

        let mut strategy_counts = std::collections::HashMap::new();
        for action in &self.actions {
            *strategy_counts.entry(action.strategy).or_insert(0) += 1;
        }

        RecoveryStats {
            total_actions,
            total_tokens_skipped,
            strategy_counts,
        }
    }
}

impl Default for RecoveryManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about error recovery.
#[derive(Debug, Clone)]
pub struct RecoveryStats {
    /// Total number of recovery actions taken
    pub total_actions: usize,
    /// Total number of tokens skipped during recovery
    pub total_tokens_skipped: usize,
    /// Count of each recovery strategy used
    pub strategy_counts: std::collections::HashMap<RecoveryStrategy, usize>,
}

impl RecoveryStats {
    /// Check if any recovery actions were taken.
    pub fn has_recoveries(&self) -> bool {
        self.total_actions > 0
    }

    /// Get a human-readable summary.
    pub fn summary(&self) -> String {
        if !self.has_recoveries() {
            return "No error recovery actions taken.".to_string();
        }

        let mut lines = vec![
            format!("Error recovery summary:"),
            format!("  Total actions: {}", self.total_actions),
            format!("  Tokens skipped: {}", self.total_tokens_skipped),
            format!("  Strategies used:"),
        ];

        for (strategy, count) in &self.strategy_counts {
            lines.push(format!("    {:?}: {}", strategy, count));
        }

        lines.join("\n")
    }
}
