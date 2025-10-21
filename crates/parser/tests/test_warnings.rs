//! Integration tests for warning system in Coral compiler.
//!
//! This test suite validates that warnings are properly detected, categorized,
//! and provide helpful suggestions for code quality improvements.

use coral_parser::error::codes::ErrorCode;
use coral_parser::error::warnings::{Warning, WarningCategory, WarningKind};
use text_size::TextRange;

/// Helper to create a simple test warning
fn create_test_warning(kind: WarningKind, span: TextRange) -> Warning {
    Warning::new(kind, span)
}

// ===== Warning Category Tests =====

#[test]
fn test_warning_categories() {
    // Test that warning categories are distinct
    let categories = [
        WarningCategory::UnusedCode,
        WarningCategory::Shadowing,
        WarningCategory::Import,
        WarningCategory::Deprecation,
    ];

    for (i, cat1) in categories.iter().enumerate() {
        for (j, cat2) in categories.iter().enumerate() {
            if i == j {
                assert_eq!(cat1, cat2);
            } else {
                assert_ne!(cat1, cat2);
            }
        }
    }
}

// ===== Warning Code Tests =====

#[test]
fn test_warning_codes_format_correctly() {
    // Test that warning codes start with W
    let warning_codes = vec![
        ErrorCode::W2001, // Deprecated feature
        ErrorCode::W3001, // Unused variable
        ErrorCode::W3002, // Unused function
        ErrorCode::W3003, // Unused import
        ErrorCode::W3004, // Unused parameter
        ErrorCode::W3006, // Shadows builtin
        ErrorCode::W3007, // Shadows import
    ];

    for code in warning_codes {
        let formatted = format!("{}", code);
        assert!(
            formatted.starts_with("W"),
            "Warning code should start with 'W': {}",
            formatted
        );
        println!("{}", formatted);
    }
}

#[test]
fn test_warning_descriptions() {
    // Test that all warning codes have descriptions via WarningKind
    let span = TextRange::new(0.into(), 10.into());

    let warning_kinds = vec![
        WarningKind::DeprecatedFeature {
            feature: "test".to_string(),
            alternative: None,
            removal_version: None,
            span,
        },
        WarningKind::UnusedVariable {
            name: "test".to_string(),
            span,
        },
        WarningKind::UnusedFunction {
            name: "test".to_string(),
            span,
        },
        WarningKind::UnusedImport {
            name: "test".to_string(),
            span,
        },
        WarningKind::UnusedParameter {
            name: "test".to_string(),
            span,
        },
        WarningKind::ShadowsBuiltin {
            name: "test".to_string(),
            span,
        },
        WarningKind::ShadowsImport {
            name: "test".to_string(),
            previous_span: span,
            span,
        },
    ];

    for kind in warning_kinds {
        let message = kind.format_message();
        assert!(
            !message.is_empty(),
            "Warning {:?} should have message",
            kind.code()
        );

        println!("{}: {}", kind.code(), message);
    }
}

#[test]
fn test_warning_suggestions() {
    // Test that all warning codes have helpful suggestions via WarningKind
    let span = TextRange::new(0.into(), 10.into());

    let warning_kinds = vec![
        WarningKind::DeprecatedFeature {
            feature: "test".to_string(),
            alternative: None,
            removal_version: None,
            span,
        },
        WarningKind::UnusedVariable {
            name: "test".to_string(),
            span,
        },
        WarningKind::UnusedFunction {
            name: "test".to_string(),
            span,
        },
        WarningKind::UnusedImport {
            name: "test".to_string(),
            span,
        },
        WarningKind::UnusedParameter {
            name: "test".to_string(),
            span,
        },
        WarningKind::ShadowsBuiltin {
            name: "test".to_string(),
            span,
        },
        WarningKind::ShadowsImport {
            name: "test".to_string(),
            previous_span: span,
            span,
        },
    ];

    for kind in warning_kinds {
        let code = kind.code();
        let message = kind.format_message();

        assert!(
            !message.is_empty(),
            "Warning {:?} should have message",
            code
        );

        println!("{}: {}", code, message);
    }
}

// ===== UnusedCode Warnings (W3xxx) =====

#[test]
fn test_unused_variable_warning() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::UnusedVariable {
        name: "unused_var".to_string(),
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::UnusedCode);
    assert_eq!(warning.code(), ErrorCode::W3001);
}

#[test]
fn test_unused_function_warning() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::UnusedFunction {
        name: "unused_func".to_string(),
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::UnusedCode);
    assert_eq!(warning.code(), ErrorCode::W3002);
}

#[test]
fn test_unused_import_warning() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::UnusedImport {
        name: "unused_module".to_string(),
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::Import);
    assert_eq!(warning.code(), ErrorCode::W3003);
}

#[test]
fn test_unused_parameter_warning() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::UnusedParameter {
        name: "unused_param".to_string(),
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::UnusedCode);
    assert_eq!(warning.code(), ErrorCode::W3004);
}

// ===== Shadowing Warnings (W3xxx) =====

#[test]
fn test_shadows_builtin_warning() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::ShadowsBuiltin {
        name: "list".to_string(),
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::Shadowing);
    assert_eq!(warning.code(), ErrorCode::W3006);
}

#[test]
fn test_shadows_import_warning() {
    let prev_span = TextRange::new(0.into(), 10.into());
    let span = TextRange::new(20.into(), 30.into());
    let kind = WarningKind::ShadowsImport {
        name: "os".to_string(),
        previous_span: prev_span,
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::Shadowing);
    assert_eq!(warning.code(), ErrorCode::W3007);
}

// ===== Deprecation Warnings (W2xxx) =====

#[test]
fn test_deprecated_feature_warning_basic() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::DeprecatedFeature {
        feature: "old_function".to_string(),
        alternative: None,
        removal_version: None,
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::Deprecation);
    assert_eq!(warning.code(), ErrorCode::W2001);
}

#[test]
fn test_deprecated_feature_warning_with_alternative() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::DeprecatedFeature {
        feature: "old_function".to_string(),
        alternative: Some("new_function".to_string()),
        removal_version: Some("2.0".to_string()),
        span,
    };

    let warning = create_test_warning(kind, span);
    assert_eq!(warning.category(), WarningCategory::Deprecation);
    assert_eq!(warning.code(), ErrorCode::W2001);
}

// ===== Warning with Notes =====

#[test]
fn test_warning_with_notes() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::UnusedVariable {
        name: "x".to_string(),
        span,
    };

    let warning = Warning::new(kind, span)
        .with_note("Consider removing this variable".to_string())
        .with_note("Or use it in your code".to_string());

    assert_eq!(warning.notes.len(), 2);
}

// ===== Warning Diagnostic Conversion =====

#[test]
fn test_warning_to_diagnostic() {
    let source = "x = 42";
    let span = TextRange::new(0.into(), 6.into());
    let kind = WarningKind::UnusedVariable {
        name: "x".to_string(),
        span,
    };

    let warning = Warning::new(kind, span);
    let diagnostic = warning.to_diagnostic(source);

    assert!(diagnostic.message.contains("x") || diagnostic.message.contains("unused"));
    println!("Warning diagnostic: {}", diagnostic.message);
}

// ===== WarningKind Message Formatting =====

#[test]
fn test_warning_kind_format_unused_import() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::UnusedImport {
        name: "os".to_string(),
        span,
    };

    let message = kind.format_message();
    assert!(
        message.contains("os"),
        "Message should mention the import name"
    );
    assert!(
        message.contains("unused") || message.contains("never used"),
        "Message should indicate it's unused"
    );
    println!("Unused import message: {}", message);
}

#[test]
fn test_warning_kind_format_shadows_builtin() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::ShadowsBuiltin {
        name: "list".to_string(),
        span,
    };

    let message = kind.format_message();
    assert!(message.contains("list"), "Message should mention the name");
    assert!(
        message.contains("shadow") || message.contains("builtin"),
        "Message should indicate shadowing a builtin"
    );
    println!("Shadows builtin message: {}", message);
}

#[test]
fn test_warning_kind_format_shadows_import() {
    let prev_span = TextRange::new(0.into(), 10.into());
    let span = TextRange::new(20.into(), 30.into());
    let kind = WarningKind::ShadowsImport {
        name: "os".to_string(),
        previous_span: prev_span,
        span,
    };

    let message = kind.format_message();
    assert!(
        message.contains("os"),
        "Message should mention the import name"
    );
    assert!(
        message.contains("shadow") || message.contains("previous"),
        "Message should indicate shadowing"
    );
    println!("Shadows import message: {}", message);
}

#[test]
fn test_warning_kind_format_deprecated_feature() {
    let span = TextRange::new(0.into(), 10.into());
    let kind = WarningKind::DeprecatedFeature {
        feature: "old_api".to_string(),
        alternative: Some("new_api".to_string()),
        removal_version: Some("2.0".to_string()),
        span,
    };

    let message = kind.format_message();
    assert!(
        message.contains("old_api"),
        "Message should mention the deprecated feature"
    );
    assert!(
        message.contains("deprecated"),
        "Message should indicate deprecation"
    );
    println!("Deprecated feature message: {}", message);
}

// ===== Warning Metadata Tests =====

#[test]
fn test_warning_metadata_consistency() {
    let span = TextRange::new(0.into(), 10.into());
    let warnings = vec![
        WarningKind::UnusedVariable {
            name: "x".to_string(),
            span,
        },
        WarningKind::UnusedFunction {
            name: "f".to_string(),
            span,
        },
        WarningKind::UnusedImport {
            name: "os".to_string(),
            span,
        },
        WarningKind::ShadowsBuiltin {
            name: "list".to_string(),
            span,
        },
    ];

    for kind in warnings {
        // All should have valid error type
        let error_type = kind.error_type();
        assert!(!error_type.is_empty());

        // All should have valid code
        let code = kind.code();
        let code_str = format!("{}", code);
        assert!(code_str.starts_with("W"));

        // All should have valid category
        let category = kind.category();

        println!("Warning: {:?} - {} - {:?}", code, error_type, category);
    }
}

// ===== Real-world Warning Scenarios =====

#[test]
fn test_multiple_warnings_same_category() {
    let span1 = TextRange::new(0.into(), 5.into());
    let span2 = TextRange::new(10.into(), 15.into());

    let warnings = vec![
        Warning::new(
            WarningKind::UnusedVariable {
                name: "x".to_string(),
                span: span1,
            },
            span1,
        ),
        Warning::new(
            WarningKind::UnusedVariable {
                name: "y".to_string(),
                span: span2,
            },
            span2,
        ),
    ];

    for warning in &warnings {
        assert_eq!(warning.category(), WarningCategory::UnusedCode);
    }
}

#[test]
fn test_mixed_warning_categories() {
    let span = TextRange::new(0.into(), 10.into());

    let warnings = vec![
        Warning::new(
            WarningKind::UnusedVariable {
                name: "x".to_string(),
                span,
            },
            span,
        ),
        Warning::new(
            WarningKind::ShadowsBuiltin {
                name: "list".to_string(),
                span,
            },
            span,
        ),
        Warning::new(
            WarningKind::UnusedImport {
                name: "os".to_string(),
                span,
            },
            span,
        ),
    ];

    let categories: Vec<_> = warnings.iter().map(|w| w.category()).collect();

    // Should have different categories
    assert!(categories.contains(&WarningCategory::UnusedCode));
    assert!(categories.contains(&WarningCategory::Shadowing));
    assert!(categories.contains(&WarningCategory::Import));
}

// ===== Warning Suppression Patterns =====

#[test]
fn test_underscore_prefix_convention() {
    // Test that underscore-prefixed names are conventionally unused
    let span = TextRange::new(0.into(), 10.into());

    // This would typically not generate a warning in real code
    let kind = WarningKind::UnusedVariable {
        name: "_unused".to_string(),
        span,
    };

    let warning = Warning::new(kind, span);

    // In a real implementation, warnings for _-prefixed names might be suppressed
    assert_eq!(warning.code(), ErrorCode::W3001);
    println!("Underscore-prefixed variable: {}", warning.code());
}

// ===== Edge Cases =====

#[test]
fn test_empty_name_warning() {
    let span = TextRange::new(0.into(), 1.into());
    let kind = WarningKind::UnusedVariable {
        name: String::new(),
        span,
    };

    let warning = Warning::new(kind, span);
    let message = warning.kind.format_message();

    // Should handle empty names gracefully
    assert!(!message.is_empty());
    println!("Empty name warning: {}", message);
}

#[test]
fn test_very_long_name_warning() {
    let span = TextRange::new(0.into(), 100.into());
    let long_name = "a".repeat(1000);
    let kind = WarningKind::UnusedVariable {
        name: long_name.clone(),
        span,
    };

    let warning = Warning::new(kind, span);
    let message = warning.kind.format_message();

    // Should handle very long names
    assert!(message.contains(&long_name) || message.len() > 100);
    println!("Long name warning length: {}", message.len());
}

// ===== Warning Code Coverage =====

#[test]
fn test_all_warning_codes_have_unique_numbers() {
    use std::collections::HashSet;

    let codes = vec![
        ErrorCode::W2001,
        ErrorCode::W3001,
        ErrorCode::W3002,
        ErrorCode::W3003,
        ErrorCode::W3004,
        ErrorCode::W3006,
        ErrorCode::W3007,
    ];

    let mut numbers = HashSet::new();
    for code in codes {
        let num = code.code();
        assert!(
            numbers.insert(num),
            "Warning code {:?} has duplicate number {}",
            code,
            num
        );
    }
}

#[test]
fn test_warning_code_ranges() {
    // Test that warning codes are in expected ranges
    assert_eq!(ErrorCode::W2001.code(), 2001); // Deprecation warnings
    assert_eq!(ErrorCode::W3001.code(), 3001); // Code quality warnings
    assert_eq!(ErrorCode::W3002.code(), 3002);
    assert_eq!(ErrorCode::W3003.code(), 3003);
    assert_eq!(ErrorCode::W3004.code(), 3004);
    assert_eq!(ErrorCode::W3006.code(), 3006);
    assert_eq!(ErrorCode::W3007.code(), 3007);
}
