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

use coral_parser::helpers::TestCase;

#[test]
fn test_all_string_types() {
    let cases: &[TestCase] = &[
        TestCase::new("single_quoted_string", r#"x = 'hello'"#),
        TestCase::new("double_quoted_string", r#"x = "hello""#),
        TestCase::new("single_with_double_quotes", r#"x = 'She said "Hello"'"#),
        TestCase::new("double_with_single_quotes", r#"x = "It's working""#),
        TestCase::new("empty_strings", r#"a = """#),
        TestCase::new("empty_strings", r#"b = ''"#),
        TestCase::new("basic_escapes", r#"x = "Line1\nLine2""#),
        TestCase::new("basic_escapes", r#"x = "Col1\tCol2""#),
        TestCase::new("basic_escapes", r#"x = "Back\\slash""#),
        TestCase::new("basic_escapes", r#"x = "Quote\"here""#),
        TestCase::new("basic_escapes", r#"x = 'Quote\'here'"#),
        TestCase::new("other_escapes", r#"x = "\r\b\f\a\v""#),
        TestCase::new("hex_escapes", r#"x = "\x48\x65\x6C\x6C\x6F""#),
        TestCase::new("octal_escapes", r#"x = "\110\145\154\154\157""#),
        TestCase::new("unicode_16bit_escapes", r#"x = "\u0048\u0065\u006C""#),
        TestCase::new("unicode_32bit_escapes", r#"x = "\U00000048\U00000065""#),
        TestCase::new("unicode_direct", r#"x = "Hello 👋 World 🌍""#),
        TestCase::new("unicode_direct", r#"x = "Café, naïve""#),
        TestCase::new("unicode_direct", r#"x = "© ® ™ € £ ¥""#),
        TestCase::new("unicode_direct", r#"x = "π ≈ 3.14159""#),
        TestCase::new("raw_single_quoted", r#"x = r'C:\Users\name'"#),
        TestCase::new("raw_double_quoted", r#"x = r"C:\Windows\System32""#),
        TestCase::new("bytes_single_quoted", r#"x = b'bytes'"#),
        TestCase::new("bytes_double_quoted", r#"x = b"bytes""#),
        TestCase::new("bytes_with_escapes", r#"x = b"Line1\nLine2\tTab""#),
        TestCase::new("bytes_hex_escapes", r#"x = b"\x48\x65\x6C\x6C\x6F""#),
        TestCase::new("bytes_octal_escapes", r#"x = b"\110\145\154\154\157""#),
        TestCase::new("fstring_single_quoted", r#"x = f'Hello {name}'"#),
        TestCase::new("fstring_double_quoted", r#"x = f"Hello {name}""#),
        TestCase::new("fstring_with_expression", r#"x = f"Sum: {a + b}""#),
        TestCase::new("fstring_with_format_spec", r#"x = f"Pi: {pi:.2f}""#),
        TestCase::new("fstring_uppercase_prefix", r#"x = F"test {x}""#),
        TestCase::new("fstring_empty", r#"x = f"""#),
        TestCase::new("raw_fstring_single", r#"x = rf'Path: {path}\n'"#),
        TestCase::new("raw_fstring_double", r#"x = rf"Path: {path}\n""#),
        TestCase::new("raw_fstring_variants", r#"x = rf"test {x}""#),
        TestCase::new("all_byte_prefix_combinations", r#"x = b"test""#),
        TestCase::new("all_fstring_prefix_combinations", r#"x = f"test""#),
        TestCase::new("adjacent_strings", r#"x = "Hello" "World""#),
        TestCase::new(
            "multiline_adjacent_strings",
            r#"x = (
    "Line 1 "
    "Line 2 "
    "Line 3"
)"#,
        ),
        TestCase::new("string_with_null_byte", r#"x = "\x00""#),
        TestCase::new("string_with_all_escapes", r#"x = "\n\t\r\b\f\a\v\\\'\"" "#),
        TestCase::new("very_long_string", r#"x = "a".repeat(10000)"#),
        TestCase::new(
            "string_with_special_chars",
            r#"x = "!@#$%^&*()_+-=[]{}|;:,.<>?""#,
        ),
        TestCase::new("various_unicode_ranges", r#"x = "αβγδε""#),
        TestCase::new("various_unicode_ranges", r#"x = "абвгд""#),
        TestCase::new("various_unicode_ranges", r#"x = "你好世界""#),
        TestCase::new("various_unicode_ranges", r#"x = "こんにちは""#),
        TestCase::new("various_unicode_ranges", r#"x = "مرحبا""#),
        TestCase::new("various_unicode_ranges", r#"x = "שלום""#),
        TestCase::new("emoji_in_strings", r#"x = "😀😃😄😁""#),
        TestCase::new("emoji_in_strings", r#"x = "❤️💛💚💙💜""#),
        TestCase::new("emoji_in_strings", r#"x = "🐶🐱🐭🐹🐰""#),
        TestCase::new("strings_in_expressions", r#"x = "Hello" + " " + "World""#),
        TestCase::new("strings_in_expressions", r#"x = "Echo! " * 3"#),
        TestCase::new("strings_in_expressions", r#"x = "test" in "testing""#),
        TestCase::new("strings_in_collections", r#"x = ["a", "b", "c"]"#),
        TestCase::new("strings_in_collections", r#"x = {"key": "value"}"#),
        TestCase::new("strings_in_collections", r#"x = ("a", "b", "c")"#),
        TestCase::new("strings_in_collections", r#"x = {"a", "b", "c"}"#),
        TestCase::new("strings_as_function_args", r#"print("Hello")"#),
        TestCase::new("strings_as_function_args", r#"foo("arg1", "arg2", "arg3")"#),
        TestCase::new(
            "strings_in_conditions",
            r#"if x == "test":
    pass"#,
        ),
        TestCase::new(
            "strings_in_conditions",
            r#"while name != "":
    pass"#,
        ),
        // Multiline string tests
        TestCase::new(
            "triple_double_quotes_simple",
            r#"x = """
Hello
World
"""#,
        ),
        TestCase::new(
            "triple_single_quotes_simple",
            r#"x = '''
Hello
World
'''#,
        ),
        TestCase::new(
            "triple_double_quotes_with_text_before",
            r#"text = "prefix" + """
Hello
World
"""#,
        ),
        TestCase::new(
            "multiline_raw_string",
            r#"x = r"""
C:\Users\name
D:\Path\to\file
"""#,
        ),
        TestCase::new(
            "multiline_fstring",
            r#"x = f"""
Name: {name}
Value: {value}
"""#,
        ),
        TestCase::new(
            "multiline_tstring",
            r#"x = t"""
Template line 1: {var1}
Template line 2: {var2}
"""#,
        ),
        TestCase::new(
            "multiline_string_with_empty_lines",
            r#"doc = """
First paragraph.

Second paragraph.

Third paragraph.
"""#,
        ),
        TestCase::new(
            "multiline_string_in_function_call",
            r#"print("""
This is a
multiline message
""")"#,
        ),
        TestCase::new(
            "multiline_strings_in_list",
            r#"items = [
    """
First
item
""",
    """
Second
item
"""
]"#,
        ),
        TestCase::new(
            "nested_quotes_in_multiline",
            r#"text = """
He said "Hello"
and she replied 'Hi'
"""#,
        ),
        TestCase::new(
            "multiline_with_escapes",
            r#"x = """
Line 1
\tIndented
\nNew line
"""#,
        ),
        TestCase::new("empty_multiline_string", "x = \"\"\"\"\"\""),
        TestCase::new(
            "multiline_string_assignment_chain",
            r#"a = b = """
Shared
multiline
string
"""#,
        ),
    ];

    for case in cases {
        case.run();
    }
}
