//! Closure capture analysis
//!
//! Analyzes which variables are captured by closures and determines capture semantics
//! (by value vs by reference). This analysis supports:
//! - Lambda expressions
//! - Comprehensions (list, dict, set, generator)
//! - Nested closures

use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaptureKind {
    ByValue,
    ByReference,
    Immutable,
}

#[derive(Debug, Clone)]
pub struct ClosureCapture {
    pub name: String,
    pub kind: CaptureKind,
}

#[derive(Debug, Clone)]
pub struct ClosureAnalysis {
    pub captures: Vec<ClosureCapture>,
    pub free_vars: HashSet<String>,
    pub closed_vars: HashSet<String>,
}

impl ClosureAnalysis {
    pub fn new() -> Self {
        Self {
            captures: Vec::new(),
            free_vars: HashSet::new(),
            closed_vars: HashSet::new(),
        }
    }

    pub fn add_capture(&mut self, name: String, kind: CaptureKind) {
        if !self.captures.iter().any(|c| c.name == name) {
            self.captures.push(ClosureCapture { name, kind });
        }
    }

    pub fn add_free_var(&mut self, name: String) {
        self.free_vars.insert(name);
    }

    pub fn add_closed_var(&mut self, name: String) {
        self.closed_vars.insert(name);
    }

    pub fn is_variable_captured(&self, name: &str) -> bool {
        self.captures.iter().any(|c| c.name == name)
    }

    pub fn capture_count(&self) -> usize {
        self.captures.len()
    }
}

impl Default for ClosureAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ClosureAnalyzer {
    local_vars: HashSet<String>,
    captured_vars: HashMap<String, CaptureKind>,
    free_vars: HashSet<String>,
}

impl ClosureAnalyzer {
    pub fn new() -> Self {
        Self {
            local_vars: HashSet::new(),
            captured_vars: HashMap::new(),
            free_vars: HashSet::new(),
        }
    }

    pub fn add_local(&mut self, name: String) {
        self.local_vars.insert(name);
    }

    pub fn add_local_many(&mut self, names: impl IntoIterator<Item = String>) {
        for name in names {
            self.local_vars.insert(name);
        }
    }

    pub fn reference_variable(&mut self, name: String, is_mutation: bool) {
        if !self.local_vars.contains(&name) {
            let kind = if is_mutation {
                CaptureKind::ByReference
            } else {
                CaptureKind::ByValue
            };
            self.captured_vars.insert(name.clone(), kind);
            self.free_vars.insert(name);
        }
    }

    pub fn finalize(&self) -> ClosureAnalysis {
        let mut analysis = ClosureAnalysis::new();

        for (name, kind) in &self.captured_vars {
            analysis.add_capture(name.clone(), kind.clone());
        }

        for name in &self.free_vars {
            analysis.add_free_var(name.clone());
        }

        for name in &self.local_vars {
            if !self.captured_vars.contains_key(name) {
                analysis.add_closed_var(name.clone());
            }
        }

        analysis
    }

    pub fn merge(&mut self, other: &ClosureAnalyzer) {
        for (name, kind) in &other.captured_vars {
            if !self.local_vars.contains(name) {
                self.captured_vars.insert(name.clone(), kind.clone());
                self.free_vars.insert(name.clone());
            }
        }
    }
}

impl Default for ClosureAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_closure_analysis_creation() {
        let analysis = ClosureAnalysis::new();
        assert_eq!(analysis.capture_count(), 0);
        assert!(analysis.free_vars.is_empty());
    }

    #[test]
    fn test_add_capture() {
        let mut analysis = ClosureAnalysis::new();
        analysis.add_capture("x".to_string(), CaptureKind::ByValue);

        assert!(analysis.is_variable_captured("x"));
        assert_eq!(analysis.capture_count(), 1);
    }

    #[test]
    fn test_analyzer_local_vars() {
        let mut analyzer = ClosureAnalyzer::new();
        analyzer.add_local("x".to_string());
        analyzer.add_local("y".to_string());

        analyzer.reference_variable("x".to_string(), false);
        analyzer.reference_variable("z".to_string(), false);

        let analysis = analyzer.finalize();
        assert_eq!(analysis.capture_count(), 1);
        assert!(analysis.captures[0].name == "z");
    }

    #[test]
    fn test_analyzer_mutation_tracking() {
        let mut analyzer = ClosureAnalyzer::new();
        analyzer.reference_variable("x".to_string(), true);

        let analysis = analyzer.finalize();
        assert_eq!(analysis.capture_count(), 1);
        assert_eq!(analysis.captures[0].kind, CaptureKind::ByReference);
    }

    #[test]
    fn test_analyzer_merge() {
        let mut analyzer1 = ClosureAnalyzer::new();
        analyzer1.add_local("a".to_string());
        analyzer1.reference_variable("b".to_string(), false);

        let mut analyzer2 = ClosureAnalyzer::new();
        analyzer2.reference_variable("b".to_string(), false);
        analyzer2.reference_variable("c".to_string(), false);

        analyzer1.merge(&analyzer2);
        let analysis = analyzer1.finalize();

        assert!(analysis.captures.len() >= 2);
        assert!(analysis.free_vars.contains("b"));
        assert!(analysis.free_vars.contains("c"));
    }
}
