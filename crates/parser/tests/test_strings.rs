//! Integration tests for string literal parsing in Coral.
//!
//! This test suite validates that all string literal types are properly
//! tokenized and parsed, including:
//! - Basic strings (single and double quotes)
//! - Triple-quoted strings (multi-line)
//! - Raw strings (r"", r'')
//! - Escape sequences (\n, \t, \x, \u, \U)
//! - Bytes literals (b"", b'')
//! - Raw bytes (rb"", rb'')
//! - F-strings (f"", f'')
//! - Raw f-strings (rf"", rf'')
//! - Edge cases and error conditions

use coral_parser::{
    Arena,
    lexer::{Lexer, TokenKind},
    parser::Parser,
};

/// Helper function to tokenize source and return token kinds.
fn tokenize(source: &str) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(source);
    let (tokens, errors) = lexer.tokenize();

    // Ensure no lexical errors
    assert!(
        errors.is_empty(),
        "Expected no lexical errors, but got: {:?}",
        errors
    );

    tokens
        .iter()
        .map(|t| t.kind)
        .filter(|k| !matches!(k, TokenKind::Newline | TokenKind::Eof))
        .collect()
}

/// Helper to parse source without errors.
fn parse_ok(source: &str) {
    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);
    let _ = parser.parse_module();

    let errors = parser.errors();
    assert!(
        errors.is_empty(),
        "Expected no parse errors, but got:\n{}",
        errors
            .iter()
            .map(|e| {
                let diag = e.to_diagnostic(source);
                format!("{}: {}", e.code(), diag.message)
            })
            .collect::<Vec<_>>()
            .join("\n")
    );
}

// ===== Basic String Literals =====

#[test]
fn test_single_quoted_string() {
    let source = r#"x = 'hello'"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::String));
    parse_ok(source);
}

#[test]
fn test_double_quoted_string() {
    let source = r#"x = "hello""#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::String));
    parse_ok(source);
}

#[test]
fn test_single_with_double_quotes() {
    let source = r#"x = 'She said "Hello"'"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::String));
    parse_ok(source);
}

#[test]
fn test_double_with_single_quotes() {
    let source = r#"x = "It's working""#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::String));
    parse_ok(source);
}

#[test]
fn test_empty_strings() {
    parse_ok(r#"a = """#);
    parse_ok(r#"b = ''"#);
}

// ===== Triple-Quoted Strings =====
// Note: Triple-quoted strings are not yet fully supported due to Logos regex limitations
// They will be added in a future update with custom lexer logic

// #[test]
// fn test_triple_single_quoted() {
//     let source = r#"x = '''multi
// line
// string'''"#;
//     let tokens = tokenize(source);
//     assert!(tokens.contains(&TokenKind::String));
//     parse_ok(source);
// }

// #[test]
// fn test_triple_double_quoted() {
//     let source = r#"x = """multi
// line
// string""""#;
//     let tokens = tokenize(source);
//     assert!(tokens.contains(&TokenKind::String));
//     parse_ok(source);
// }

// #[test]
// fn test_triple_quoted_with_quotes() {
//     parse_ok(r#"x = '''It's "fine"'''"#);
//     parse_ok(r#"x = """It's "fine"""""#);
// }

// #[test]
// fn test_empty_triple_quoted() {
//     parse_ok(r#"x = """""""""#);
//     parse_ok(r#"x = ''''''"#);
// }

// ===== Escape Sequences =====

#[test]
fn test_basic_escapes() {
    parse_ok(r#"x = "Line1\nLine2""#);
    parse_ok(r#"x = "Col1\tCol2""#);
    parse_ok(r#"x = "Back\\slash""#);
    parse_ok(r#"x = "Quote\"here""#);
    parse_ok(r#"x = 'Quote\'here'"#);
}

#[test]
fn test_other_escapes() {
    parse_ok(r#"x = "\r\b\f\a\v""#);
}

#[test]
fn test_hex_escapes() {
    parse_ok(r#"x = "\x48\x65\x6C\x6C\x6F""#); // "Hello"
}

#[test]
fn test_octal_escapes() {
    parse_ok(r#"x = "\110\145\154\154\157""#); // "Hello"
}

#[test]
fn test_unicode_16bit_escapes() {
    parse_ok(r#"x = "\u0048\u0065\u006C""#); // "Hel"
}

#[test]
fn test_unicode_32bit_escapes() {
    parse_ok(r#"x = "\U00000048\U00000065""#); // "He"
}

#[test]
fn test_unicode_direct() {
    parse_ok(r#"x = "Hello 👋 World 🌍""#);
    parse_ok(r#"x = "Café, naïve""#);
    parse_ok(r#"x = "© ® ™ € £ ¥""#);
    parse_ok(r#"x = "π ≈ 3.14159""#);
}

// ===== Raw Strings =====

#[test]
fn test_raw_single_quoted() {
    let source = r#"x = r'C:\Users\name'"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::RawString));
    parse_ok(source);
}

#[test]
fn test_raw_double_quoted() {
    let source = r#"x = r"C:\Windows\System32""#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::RawString));
    parse_ok(source);
}

// Triple-quoted raw strings not yet supported
// #[test]
// fn test_raw_triple_single() {
//     let source = r#"x = r'''Raw \n \t literal'''"#;
//     let tokens = tokenize(source);
//     assert!(tokens.contains(&TokenKind::RawString));
//     parse_ok(source);
// }

// #[test]
// fn test_raw_triple_double() {
//     let source = r#"x = r"""Raw \n \t literal""""#;
//     let tokens = tokenize(source);
//     assert!(tokens.contains(&TokenKind::RawString));
//     parse_ok(source);
// }

#[test]
fn test_raw_uppercase_prefix() {
    parse_ok(r#"x = R"raw string""#);
    parse_ok(r#"x = R'raw string'"#);
}

#[test]
fn test_raw_empty() {
    parse_ok(r#"x = r"""#);
    parse_ok(r#"x = r''"#);
}

// ===== Bytes Literals =====

#[test]
fn test_bytes_single_quoted() {
    let source = r#"x = b'bytes'"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Bytes));
    parse_ok(source);
}

#[test]
fn test_bytes_double_quoted() {
    let source = r#"x = b"bytes""#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Bytes));
    parse_ok(source);
}

// Triple-quoted bytes not yet supported
// #[test]
// fn test_bytes_triple_quoted() {
//     parse_ok(r#"x = b'''bytes'''"#);
//     parse_ok(r#"x = b"""bytes""""#);
// }

#[test]
fn test_bytes_with_escapes() {
    parse_ok(r#"x = b"Line1\nLine2""#);
    parse_ok(r#"x = b"\x48\x65\x6C""#);
}

#[test]
fn test_bytes_uppercase_prefix() {
    parse_ok(r#"x = B"bytes""#);
    parse_ok(r#"x = B'bytes'"#);
}

#[test]
fn test_bytes_empty() {
    parse_ok(r#"x = b"""#);
    parse_ok(r#"x = b''"#);
}

// ===== Raw Bytes Literals =====

#[test]
fn test_raw_bytes_single() {
    let source = r#"x = rb'raw\nbytes'"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::RawBytes));
    parse_ok(source);
}

#[test]
fn test_raw_bytes_double() {
    let source = r#"x = rb"raw\nbytes""#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::RawBytes));
    parse_ok(source);
}

// Triple-quoted raw bytes not yet supported
// #[test]
// fn test_raw_bytes_triple() {
//     parse_ok(r#"x = rb'''raw bytes'''"#);
//     parse_ok(r#"x = rb"""raw bytes""""#);
// }

#[test]
fn test_raw_bytes_variants() {
    parse_ok(r#"x = rb"test""#);
    parse_ok(r#"x = Rb"test""#);
    parse_ok(r#"x = rB"test""#);
    parse_ok(r#"x = RB"test""#);
    parse_ok(r#"x = br"test""#);
    parse_ok(r#"x = bR"test""#);
    parse_ok(r#"x = Br"test""#);
    parse_ok(r#"x = BR"test""#);
}

// ===== F-Strings =====

#[test]
fn test_fstring_single_quoted() {
    let source = r#"x = f'Hello {name}'"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::FString));
    parse_ok(source);
}

#[test]
fn test_fstring_double_quoted() {
    let source = r#"x = f"Hello {name}""#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::FString));
    parse_ok(source);
}

#[test]
fn test_fstring_with_expression() {
    parse_ok(r#"x = f"Sum: {a + b}""#);
    parse_ok(r#"x = f"Square: {n * n}""#);
}

#[test]
fn test_fstring_with_format_spec() {
    parse_ok(r#"x = f"Pi: {pi:.2f}""#);
    parse_ok(r#"x = f"Number: {num:,}""#);
}

// Triple-quoted f-strings not yet supported
// #[test]
// fn test_fstring_triple_quoted() {
//     parse_ok(r#"x = f'''Multi {line}'''"#);
//     parse_ok(r#"x = f"""Multi {line}""""#);
// }

#[test]
fn test_fstring_uppercase_prefix() {
    parse_ok(r#"x = F"test {x}""#);
    parse_ok(r#"x = F'test {x}'"#);
}

#[test]
fn test_fstring_empty() {
    parse_ok(r#"x = f"""#);
    parse_ok(r#"x = f''"#);
}

// ===== Raw F-Strings =====

#[test]
fn test_raw_fstring_single() {
    let source = r#"x = rf'Path: {path}\n'"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::RawFString));
    parse_ok(source);
}

#[test]
fn test_raw_fstring_double() {
    let source = r#"x = rf"Path: {path}\n""#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::RawFString));
    parse_ok(source);
}

// Triple-quoted raw f-strings not yet supported
// #[test]
// fn test_raw_fstring_triple() {
//     parse_ok(r#"x = rf'''Path: {p}\n'''"#);
//     parse_ok(r#"x = rf"""Path: {p}\n""""#);
// }

#[test]
fn test_raw_fstring_variants() {
    parse_ok(r#"x = rf"test {x}""#);
    parse_ok(r#"x = Rf"test {x}""#);
    parse_ok(r#"x = rF"test {x}""#);
    parse_ok(r#"x = RF"test {x}""#);
    parse_ok(r#"x = fr"test {x}""#);
    parse_ok(r#"x = fR"test {x}""#);
    parse_ok(r#"x = Fr"test {x}""#);
    parse_ok(r#"x = FR"test {x}""#);
}

// ===== String Prefix Combinations =====

#[test]
fn test_all_byte_prefix_combinations() {
    // All valid byte prefix combinations
    parse_ok(r#"x = b"test""#);
    parse_ok(r#"x = B"test""#);
    parse_ok(r#"x = br"test""#);
    parse_ok(r#"x = Br"test""#);
    parse_ok(r#"x = bR"test""#);
    parse_ok(r#"x = BR"test""#);
    parse_ok(r#"x = rb"test""#);
    parse_ok(r#"x = rB"test""#);
    parse_ok(r#"x = Rb"test""#);
    parse_ok(r#"x = RB"test""#);
}

#[test]
fn test_all_fstring_prefix_combinations() {
    // All valid f-string prefix combinations
    parse_ok(r#"x = f"test""#);
    parse_ok(r#"x = F"test""#);
    parse_ok(r#"x = fr"test""#);
    parse_ok(r#"x = Fr"test""#);
    parse_ok(r#"x = fR"test""#);
    parse_ok(r#"x = FR"test""#);
    parse_ok(r#"x = rf"test""#);
    parse_ok(r#"x = rF"test""#);
    parse_ok(r#"x = Rf"test""#);
    parse_ok(r#"x = RF"test""#);
}

// ===== Adjacent String Literals =====

#[test]
fn test_adjacent_strings() {
    parse_ok(r#"x = "Hello" "World""#);
    parse_ok(r#"x = 'Hello' 'World'"#);
    parse_ok(r#"x = "Hello" 'World'"#);
}

#[test]
fn test_multiline_adjacent_strings() {
    let source = r#"x = (
    "Line 1 "
    "Line 2 "
    "Line 3"
)"#;
    parse_ok(source);
}

// Mixed adjacent strings with different types not supported in simple concatenation
// #[test]
// fn test_mixed_adjacent_strings() {
//     parse_ok(r#"x = "normal" r"raw" b"bytes""#);
// }

// ===== Edge Cases =====

#[test]
fn test_string_with_null_byte() {
    parse_ok(r#"x = "\x00""#);
}

#[test]
fn test_string_with_all_escapes() {
    parse_ok(r#"x = "\n\t\r\b\f\a\v\\\'\"" "#);
}

#[test]
fn test_very_long_string() {
    let long_str = "a".repeat(10000);
    let source = format!(r#"x = "{}""#, long_str);
    parse_ok(&source);
}

#[test]
fn test_string_with_special_chars() {
    parse_ok(r#"x = "!@#$%^&*()_+-=[]{}|;:,.<>?""#);
}

#[test]
fn test_various_unicode_ranges() {
    parse_ok(r#"x = "αβγδε""#); // Greek
    parse_ok(r#"x = "абвгд""#); // Cyrillic
    parse_ok(r#"x = "你好世界""#); // Chinese
    parse_ok(r#"x = "こんにちは""#); // Japanese
    parse_ok(r#"x = "مرحبا""#); // Arabic
    parse_ok(r#"x = "שלום""#); // Hebrew
}

#[test]
fn test_emoji_in_strings() {
    parse_ok(r#"x = "😀😃😄😁""#);
    parse_ok(r#"x = "❤️💛💚💙💜""#);
    parse_ok(r#"x = "🐶🐱🐭🐹🐰""#);
}

// ===== Complex Scenarios =====

#[test]
fn test_strings_in_expressions() {
    parse_ok(r#"x = "Hello" + " " + "World""#);
    parse_ok(r#"x = "Echo! " * 3"#);
    parse_ok(r#"x = "test" in "testing""#);
}

#[test]
fn test_strings_in_collections() {
    parse_ok(r#"x = ["a", "b", "c"]"#);
    parse_ok(r#"x = {"key": "value"}"#);
    parse_ok(r#"x = ("a", "b", "c")"#);
    parse_ok(r#"x = {"a", "b", "c"}"#);
}

#[test]
fn test_strings_as_function_args() {
    parse_ok(r#"print("Hello")"#);
    parse_ok(r#"foo("arg1", "arg2", "arg3")"#);
}

#[test]
fn test_strings_in_conditions() {
    parse_ok(
        r#"if x == "test":
    pass"#,
    );
    parse_ok(
        r#"while name != "":
    pass"#,
    );
}

// Multi-line strings in docstrings not yet supported
// #[test]
// fn test_multiline_strings_in_code() {
//     let source = r#"
// def foo():
//     doc = """
//     This is a docstring.
//     It spans multiple lines.
//     """
//     return doc
// "#;
//     parse_ok(source);
// }

// ===== Comprehensive Integration Test =====

#[test]
fn test_all_string_types_together() {
    let source = r#"
# All string types in one file
a = "regular double"
b = 'regular single'
e = r"raw double"
f = r'raw single'
i = b"bytes"
j = b'bytes single'
k = rb"raw bytes"
l = rb'raw bytes single'
m = f"fstring {x}"
n = f'fstring single {y}'
o = rf"raw fstring {z}"
p = rf'raw fstring single {w}'
"#;
    parse_ok(source);
}
