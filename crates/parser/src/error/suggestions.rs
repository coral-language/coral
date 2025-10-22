//! Typo detection and suggestion system for common mistakes.

use std::collections::HashMap;

/// Common keyword typos and their corrections
pub struct TypoSuggester {
    keyword_corrections: HashMap<&'static str, &'static str>,
    operator_corrections: HashMap<&'static str, &'static str>,
}

impl TypoSuggester {
    pub fn new() -> Self {
        let mut keyword_corrections = HashMap::new();

        // Common keyword typos
        keyword_corrections.insert("elseif", "elif");
        keyword_corrections.insert("else if", "elif");
        keyword_corrections.insert("elsif", "elif");
        keyword_corrections.insert("function", "def");
        keyword_corrections.insert("func", "def");
        keyword_corrections.insert("fn", "def");
        keyword_corrections.insert("method", "def");
        keyword_corrections.insert("procedure", "def");
        keyword_corrections.insert("sub", "def");
        keyword_corrections.insert("ret", "return");
        keyword_corrections.insert("returns", "return");
        keyword_corrections.insert("printf", "print");
        keyword_corrections.insert("println", "print");
        keyword_corrections.insert("echo", "print");
        keyword_corrections.insert("const", "# Use assignment instead");
        keyword_corrections.insert("var", "# Use assignment instead");
        keyword_corrections.insert("let", "# Use assignment instead");
        keyword_corrections.insert("null", "None");
        keyword_corrections.insert("NULL", "None");
        keyword_corrections.insert("nil", "None");
        keyword_corrections.insert("undefined", "None");
        keyword_corrections.insert("true", "True");
        keyword_corrections.insert("false", "False");
        keyword_corrections.insert("boolean", "bool");
        keyword_corrections.insert("integer", "int");
        keyword_corrections.insert("string", "str");
        keyword_corrections.insert("array", "list");
        keyword_corrections.insert("hash", "dict");
        keyword_corrections.insert("hashmap", "dict");
        keyword_corrections.insert("map", "dict");
        keyword_corrections.insert("switch", "match");
        keyword_corrections.insert("case", "match");
        keyword_corrections.insert("default", "case _");
        keyword_corrections.insert("catch", "except");
        keyword_corrections.insert("try", "try");
        keyword_corrections.insert("finally", "finally");
        keyword_corrections.insert("throw", "raise");
        keyword_corrections.insert("throws", "raise");
        keyword_corrections.insert("foreach", "for");
        keyword_corrections.insert("while", "while");
        keyword_corrections.insert("do", "while");

        let mut operator_corrections = HashMap::new();

        // Common operator mistakes
        operator_corrections.insert("&&", "and");
        operator_corrections.insert("||", "or");
        operator_corrections.insert("!", "not");
        operator_corrections.insert("!==", "!=");
        operator_corrections.insert("===", "==");
        operator_corrections.insert("->", "# Use . for attribute access");
        operator_corrections.insert("=>", "# Use : in lambdas");
        operator_corrections.insert("++", "+= 1");
        operator_corrections.insert("--", "-= 1");

        Self {
            keyword_corrections,
            operator_corrections,
        }
    }

    /// Suggest a correction for a typo
    pub fn suggest_keyword(&self, typo: &str) -> Option<&'static str> {
        self.keyword_corrections.get(typo).copied()
    }

    /// Suggest a correction for an operator typo
    pub fn suggest_operator(&self, typo: &str) -> Option<&'static str> {
        self.operator_corrections.get(typo).copied()
    }

    /// Calculate Levenshtein distance for fuzzy matching
    pub fn levenshtein_distance(s1: &str, s2: &str) -> usize {
        let len1 = s1.chars().count();
        let len2 = s2.chars().count();

        if len1 == 0 {
            return len2;
        }
        if len2 == 0 {
            return len1;
        }

        let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

        for (i, row) in matrix.iter_mut().enumerate() {
            row[0] = i;
        }
        for j in 0..=len2 {
            matrix[0][j] = j;
        }

        let chars1: Vec<char> = s1.chars().collect();
        let chars2: Vec<char> = s2.chars().collect();

        for i in 1..=len1 {
            for j in 1..=len2 {
                let cost = if chars1[i - 1] == chars2[j - 1] { 0 } else { 1 };
                matrix[i][j] = std::cmp::min(
                    std::cmp::min(
                        matrix[i - 1][j] + 1, // deletion
                        matrix[i][j - 1] + 1, // insertion
                    ),
                    matrix[i - 1][j - 1] + cost, // substitution
                );
            }
        }

        matrix[len1][len2]
    }

    /// Find closest valid keyword using fuzzy matching
    pub fn suggest_closest_keyword(&self, input: &str, valid_keywords: &[&str]) -> Option<String> {
        let mut best_match = None;
        let mut best_distance = usize::MAX;

        for keyword in valid_keywords {
            let distance = Self::levenshtein_distance(input, keyword);
            // Only suggest if distance is small (1-2 characters different)
            if distance > 0 && distance <= 2 && distance < best_distance {
                best_distance = distance;
                best_match = Some(keyword.to_string());
            }
        }

        best_match
    }

    /// Suggest correction for common assignment vs comparison mistake
    pub fn suggest_assignment_vs_comparison(&self, context: &str) -> Option<&'static str> {
        // If we see = in a condition context, suggest ==
        if context.contains("if") || context.contains("while") || context.contains("elif") {
            Some("Did you mean '==' for comparison instead of '=' for assignment?")
        } else {
            None
        }
    }

    /// Suggest adding missing colon after control flow statements
    pub fn suggest_missing_colon(&self, keyword: &str) -> Option<String> {
        match keyword {
            "if" | "elif" | "else" | "for" | "while" | "def" | "class" | "try" | "except"
            | "finally" | "with" => Some(format!("Missing ':' after '{}' statement", keyword)),
            _ => None,
        }
    }

    /// Suggest adding missing parentheses for function calls
    pub fn suggest_missing_parens(&self, name: &str, context: &str) -> Option<&'static str> {
        // Common functions that need parentheses
        let function_names = [
            "print", "len", "range", "str", "int", "float", "list", "dict", "set", "tuple",
        ];

        if function_names.contains(&name) && !context.contains('(') {
            Some("Did you forget parentheses ()? Function calls require parentheses.")
        } else {
            None
        }
    }

    /// Suggest correction for common indentation mistakes
    pub fn suggest_indentation_fix(&self, error_type: &str) -> Option<&'static str> {
        match error_type {
            "mixed_tabs_spaces" => Some(
                "Use consistent indentation - either all spaces or all tabs (4 spaces is recommended)",
            ),
            "unexpected_indent" => Some(
                "Remove extra indentation or add a statement that requires indentation above this line",
            ),
            "expected_indent" => {
                Some("Add indentation (usually 4 spaces) after the ':' on the previous line")
            }
            "unindent_mismatch" => Some("Align this line with a previous indentation level"),
            _ => None,
        }
    }

    /// Suggest import for common undefined names
    pub fn suggest_import_for_name(&self, name: &str) -> Option<String> {
        let import_suggestions = [
            ("os", "import os"),
            ("sys", "import sys"),
            ("re", "import re"),
            ("json", "import json"),
            ("math", "import math"),
            ("random", "import random"),
            ("datetime", "from datetime import datetime"),
            ("Path", "from pathlib import Path"),
            ("defaultdict", "from collections import defaultdict"),
            ("Counter", "from collections import Counter"),
            ("namedtuple", "from collections import namedtuple"),
            ("OrderedDict", "from collections import OrderedDict"),
            ("sleep", "from time import sleep"),
            ("partial", "from functools import partial"),
            ("reduce", "from functools import reduce"),
        ];

        for (module_name, import_stmt) in &import_suggestions {
            if name == *module_name {
                return Some(format!("Did you forget to import? Try: {}", import_stmt));
            }
        }

        None
    }

    /// Detect if a statement looks like it's missing required syntax
    pub fn detect_missing_syntax(&self, line: &str) -> Vec<String> {
        let mut suggestions = Vec::new();

        // Missing colon after keywords
        let keywords_needing_colon = [
            "if ", "elif ", "else", "for ", "while ", "def ", "class ", "try", "except", "finally",
            "with ",
        ];

        for keyword in &keywords_needing_colon {
            if line.trim_start().starts_with(keyword) && !line.contains(':') {
                suggestions.push("Missing ':' at the end of the line".to_string());
                break;
            }
        }

        // Unclosed parentheses, brackets, braces
        let open_parens = line.chars().filter(|&c| c == '(').count();
        let close_parens = line.chars().filter(|&c| c == ')').count();
        if open_parens > close_parens {
            suggestions.push("Missing closing ')' - unclosed parenthesis".to_string());
        } else if close_parens > open_parens {
            suggestions.push("Extra closing ')' - no matching opening parenthesis".to_string());
        }

        let open_brackets = line.chars().filter(|&c| c == '[').count();
        let close_brackets = line.chars().filter(|&c| c == ']').count();
        if open_brackets > close_brackets {
            suggestions.push("Missing closing ']' - unclosed bracket".to_string());
        } else if close_brackets > open_brackets {
            suggestions.push("Extra closing ']' - no matching opening bracket".to_string());
        }

        let open_braces = line.chars().filter(|&c| c == '{').count();
        let close_braces = line.chars().filter(|&c| c == '}').count();
        if open_braces > close_braces {
            suggestions.push("Missing closing '}' - unclosed brace".to_string());
        } else if close_braces > open_braces {
            suggestions.push("Extra closing '}' - no matching opening brace".to_string());
        }

        // Unclosed quotes
        let single_quotes = line.chars().filter(|&c| c == '\'').count();
        let double_quotes = line.chars().filter(|&c| c == '"').count();
        if single_quotes % 2 == 1 {
            suggestions.push("Missing closing ' - unclosed string".to_string());
        }
        if double_quotes % 2 == 1 {
            suggestions.push("Missing closing \" - unclosed string".to_string());
        }

        suggestions
    }

    /// Suggest a correction for a misspelled attribute
    ///
    /// # Arguments
    /// * `typo` - The misspelled attribute name
    /// * `available_attrs` - List of valid attributes for the type
    /// * `type_name` - Name of the type for error message
    ///
    /// # Returns
    /// A suggestion string if a close match is found
    pub fn suggest_attribute(
        &self,
        typo: &str,
        available_attrs: &[String],
        type_name: &str,
    ) -> Option<String> {
        let mut best_match = None;
        let mut best_distance = usize::MAX;

        for attr in available_attrs {
            let distance = Self::levenshtein_distance(typo, attr);
            // Only suggest if distance is small (1-2 characters different)
            if distance > 0 && distance <= 2 && distance < best_distance {
                best_distance = distance;
                best_match = Some(attr.clone());
            }
        }

        best_match.map(|suggestion| {
            format!(
                "Type '{}' has no attribute '{}'. Did you mean '{}'?",
                type_name, typo, suggestion
            )
        })
    }

    /// Suggest a correction for a misspelled module member
    ///
    /// # Arguments
    /// * `typo` - The misspelled member name
    /// * `available_members` - List of valid members in the module
    /// * `module_name` - Name of the module for error message
    ///
    /// # Returns
    /// A suggestion string if a close match is found
    pub fn suggest_module_member(
        &self,
        typo: &str,
        available_members: &[String],
        module_name: &str,
    ) -> Option<String> {
        let mut best_match = None;
        let mut best_distance = usize::MAX;

        for member in available_members {
            let distance = Self::levenshtein_distance(typo, member);
            if distance > 0 && distance <= 2 && distance < best_distance {
                best_distance = distance;
                best_match = Some(member.clone());
            }
        }

        best_match.map(|suggestion| {
            format!(
                "Module '{}' has no member '{}'. Did you mean '{}'?",
                module_name, typo, suggestion
            )
        })
    }

    /// Suggest a correction for a misspelled type name
    ///
    /// # Arguments
    /// * `typo` - The misspelled type name
    /// * `available_types` - List of valid type names
    ///
    /// # Returns
    /// A suggestion string if a close match is found
    pub fn suggest_type_name(&self, typo: &str, available_types: &[String]) -> Option<String> {
        let mut best_match = None;
        let mut best_distance = usize::MAX;

        for type_name in available_types {
            let distance = Self::levenshtein_distance(typo, type_name);
            if distance > 0 && distance <= 2 && distance < best_distance {
                best_distance = distance;
                best_match = Some(type_name.clone());
            }
        }

        best_match
            .map(|suggestion| format!("Unknown type '{}'. Did you mean '{}'?", typo, suggestion))
    }
}

impl Default for TypoSuggester {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_suggestions() {
        let suggester = TypoSuggester::new();

        assert_eq!(suggester.suggest_keyword("elseif"), Some("elif"));
        assert_eq!(suggester.suggest_keyword("function"), Some("def"));
        assert_eq!(suggester.suggest_keyword("null"), Some("None"));
        assert_eq!(suggester.suggest_keyword("true"), Some("True"));
        assert_eq!(suggester.suggest_keyword("false"), Some("False"));
    }

    #[test]
    fn test_operator_suggestions() {
        let suggester = TypoSuggester::new();

        assert_eq!(suggester.suggest_operator("&&"), Some("and"));
        assert_eq!(suggester.suggest_operator("||"), Some("or"));
        assert_eq!(suggester.suggest_operator("!"), Some("not"));
    }

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(TypoSuggester::levenshtein_distance("kitten", "sitting"), 3);
        assert_eq!(TypoSuggester::levenshtein_distance("elif", "elseif"), 2);
        assert_eq!(TypoSuggester::levenshtein_distance("def", "def"), 0);
    }

    #[test]
    fn test_closest_keyword() {
        let suggester = TypoSuggester::new();
        let keywords = vec!["def", "class", "if", "elif", "else", "for", "while"];

        assert_eq!(
            suggester.suggest_closest_keyword("deff", &keywords),
            Some("def".to_string())
        );
        assert_eq!(
            suggester.suggest_closest_keyword("iff", &keywords),
            Some("if".to_string())
        );
    }
}
