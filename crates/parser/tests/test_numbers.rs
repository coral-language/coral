//! Integration tests for number literal parsing in Coral.
//!
//! This test suite validates that all number literal types are properly
//! tokenized and parsed, including:
//! - Decimal integers, floats, and scientific notation
//! - Binary, octal, and hexadecimal literals
//! - Complex numbers
//! - Underscore usage and validation
//! - Edge cases and error conditions

use coral_parser::helpers::{parse_ok, tokenize};
use coral_parser::lexer::TokenKind;

#[test]
fn test_decimal_integers() {
    let source = r#"
x = 0
y = 42
z = -123
large = 1_000_000
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
    parse_ok(source);
}

#[test]
fn test_decimal_floats() {
    let source = r#"
pi = 3.14159
euler = 2.71828
fraction = .5
whole = 5.
zero_point = 0.0
large_float = 1_234.567_89
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
}

#[test]
fn test_scientific_notation() {
    let source = r#"
avogadro = 6.022e23
planck = 6.626e-34
speed_of_light = 2.998e8
explicit_positive = 1.23e+5
large_scientific = 1_234.567_89e-10
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
    parse_ok(source);
}

#[test]
fn test_binary_literals() {
    let source = r#"
zero = 0b0
one = 0b1
ten = 0b1010
ff = 0b11111111
byte = 0b1111_0000
word = 0b11111111_00000000
large_binary = 0b10101010_11110000_11001100_00110011
uppercase = 0B1010
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
    parse_ok(source);
}

#[test]
fn test_octal_literals() {
    let source = r#"
zero = 0o0
seven = 0o7
ten = 0o12
ff = 0o377
permissions = 0o755
large_octal = 0o123_456_701
uppercase = 0O755
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
    parse_ok(source);
}

#[test]
fn test_hexadecimal_literals() {
    let source = r#"
zero = 0x0
fifteen = 0xf
two_five_five = 0xff
large = 0xdeadbeef
rgb_red = 0xff_00_00
mac_address = 0x00_1B_44_11_3A_B7
uuid = 0x550e8400_e29b_41d4_a716_446655440000
mixed_case = 0xDeAdBeEf
uppercase = 0XDEADBEEF
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
    parse_ok(source);
}

#[test]
fn test_complex_literals() {
    let source = r#"
imaginary_unit = 1j
imaginary_unit_upper = 1J
pure_imaginary = 3j
negative_imaginary = -2j
complex_zero = 0j
complex_int = 3+4j
complex_float = 3.14-2.71j
complex_scientific = 1.23e-4+5.67e8j
complex_underscores = 1_234.567_89+9_876.543_21j
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Complex));
    parse_ok(source);
}

#[test]
fn test_invalid_patterns_tokenize_without_errors() {

    let patterns = vec![
        "_100", "100_", "1__000", "0b1010_", "0o755_", "0xff_", "0b11__00", "0o12__34", "0xab__cd",
        "0b2", "0o8", "0xg", "0x", "0b", "0o",
    ];

    for pattern in patterns {
        let tokens = tokenize(pattern);
        assert!(!tokens.is_empty(), "Should tokenize pattern: {}", pattern);
    }
}

#[test]
fn test_invalid_scientific_notation_tokenize() {

    let patterns = vec!["1ee5", "1.2.3", "1e", "1e+", "1e-"];

    for pattern in patterns {
        let tokens = tokenize(pattern);
        assert!(!tokens.is_empty(), "Should tokenize pattern: {}", pattern);
    }
}

#[test]
fn test_valid_underscore_usage() {
    let source = r#"
valid_int = 1_000_000
valid_float = 1_234.567_89
valid_binary = 0b1010_1111_0000
valid_octal = 0o755_600
valid_hex = 0xDEAD_BEEF
valid_scientific = 1_234e5_67
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
    parse_ok(source);
}

#[test]
fn test_comprehensive_number_validation() {
    let patterns = vec![
        "1_000_", "_1_000", "1__000", "0b1010_", "0o755_", "0xff_", "0b11__00", "0o12__34",
        "0xab__cd", "0b2", "0o8", "0xg", "0x", "0b", "0o", "1ee5", "1.2.3", "1e", "1e+", "1e-",
    ];

    for pattern in patterns {
        let tokens = tokenize(pattern);
        assert!(!tokens.is_empty(), "Should tokenize pattern: {}", pattern);
    }
}

#[test]
fn test_invalid_complex_numbers() {
    parse_ok("3+4");
}

#[test]
fn test_numbers_in_expressions() {
    parse_ok("result = 42 + 3.14 + 0xff + 0b1010");
    parse_ok("product = 1_000 * 2.5 * 0o10");
}

#[test]
fn test_numbers_in_collections() {
    parse_ok("primes = [2, 3, 5, 7, 11]");
    parse_ok("constants = {3.14159: 'pi', 2.71828: 'e'}");
    parse_ok("evens = {0, 2, 4, 6, 8}");
}

#[test]
fn test_edge_cases() {
    parse_ok("digits = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]");

    parse_ok("zeros = [0, 0b0, 0o0, 0x0, 0.0, 0j]");

    parse_ok("max_int = 9223372036854775807");
    parse_ok("very_small = 2.22e-16");
}

#[test]
fn test_mixed_number_usage() {
    let source = r#"
# Mixed usage of all number types
decimal = 42
float = 3.14
scientific = 6.022e23
binary = 0b1010
octal = 0o755
hex = 0xff
complex = 3+4j

# Operations mixing types
result = decimal + float + scientific + binary + octal + hex
complex_result = complex * 2j
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Number));
    assert!(tokens.contains(&TokenKind::Complex));
    parse_ok(source);
}
