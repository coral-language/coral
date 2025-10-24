use coral_parser::helpers::DiagnosticTestBuilder;

#[test]
fn test_bool_exhaustive_with_both_cases() {
    let source = r#"
def check_bool(value):
    match value:
        case True:
            return "true"
        case False:
            return "false"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_bool_non_exhaustive_missing_false() {
    let source = r#"
def check_bool(value):
    match value:
        case True:
            return "true"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("E8001")
        .assert_some();
}

#[test]
fn test_bool_exhaustive_with_wildcard() {
    let source = r#"
def check_bool(value):
    match value:
        case True:
            return "true"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_none_exhaustive() {
    let source = r#"
def check_none(value):
    match value:
        case None:
            return "none"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_int_literals_non_exhaustive() {
    let source = r#"
def check_int(value):
    match value:
        case 0:
            return "zero"
        case 1:
            return "one"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("E8001")
        .assert_some();
}

#[test]
fn test_int_literals_exhaustive_with_wildcard() {
    let source = r#"
def check_int(value):
    match value:
        case 0:
            return "zero"
        case 1:
            return "one"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_string_literals_non_exhaustive() {
    let source = r#"
def check_string(value):
    match value:
        case "":
            return "empty"
        case "hello":
            return "greeting"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("E8001")
        .assert_some();
}

#[test]
fn test_string_exhaustive_with_wildcard() {
    let source = r#"
def check_string(value):
    match value:
        case "":
            return "empty"
        case "hello":
            return "greeting"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_redundant_wildcard_after_wildcard() {
    let source = r#"
def check_redundant(value):
    match value:
        case _:
            return "first"
        case x:
            return "second"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("Unreachable")
        .assert_some();
}

#[test]
fn test_redundant_literal_after_wildcard() {
    let source = r#"
def check_redundant(value):
    match value:
        case _:
            return "wildcard"
        case 42:
            return "forty-two"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("Unreachable")
        .assert_some();
}

#[test]
fn test_or_pattern_exhaustive() {
    let source = r#"
def check_or(value):
    match value:
        case True | False:
            return "boolean"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nested_tuple_pattern() {
    let source = r#"
def check_nested(value):
    match value:
        case (x, y):
            return "pair"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nested_list_pattern() {
    let source = r#"
def check_nested_list(value):
    match value:
        case []:
            return "empty"
        case [x]:
            return "single"
        case [x, y]:
            return "pair"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_class_pattern() {
    let source = r#"
class Point:
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

def check_point(value):
    match value:
        case Point(x, y):
            return "point"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_mapping_pattern() {
    let source = r#"
def check_dict(value):
    match value:
        case {"key": x}:
            return "has key"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_guard_doesnt_guarantee_exhaustiveness() {
    let source = r#"
def check_with_guard(value):
    match value:
        case x if x > 0:
            return "positive"
        case x if x < 0:
            return "negative"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("E8001")
        .assert_some();
}

#[test]
fn test_guard_with_wildcard_exhaustive() {
    let source = r#"
def check_with_guard_exhaustive(value):
    match value:
        case x if x > 0:
            return "positive"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_multiple_patterns_same_constructor() {
    let source = r#"
def check_multiple(value):
    match value:
        case 0:
            return "zero"
        case 1:
            return "one"
        case 2:
            return "two"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_as_pattern_with_nested() {
    let source = r#"
def check_as_pattern(value):
    match value:
        case [x, y] as pair:
            return "pair"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_deeply_nested_pattern() {
    let source = r#"
def check_deeply_nested(value):
    match value:
        case [[x, y], [a, b]]:
            return "nested"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_mixed_constructors() {
    let source = r#"
def check_mixed(value):
    match value:
        case True:
            return "true"
        case False:
            return "false"
        case None:
            return "none"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_redundant_duplicate_pattern() {
    let source = r#"
def check_duplicate(value):
    match value:
        case 42:
            return "first"
        case 42:
            return "second"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("Unreachable")
        .assert_some();
}

#[test]
fn test_complex_or_pattern() {
    let source = r#"
def check_complex_or(value):
    match value:
        case 1 | 2 | 3:
            return "small"
        case 4 | 5 | 6:
            return "medium"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_sequence_star_pattern() {
    let source = r#"
def check_star(value):
    match value:
        case []:
            return "empty"
        case [first, *rest]:
            return "non-empty"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_optional_type_pattern() {
    let source = r#"
def check_optional(value):
    match value:
        case None:
            return "none"
        case x:
            return "some"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_pattern_type_mismatch() {
    let source = r#"
def check_type_mismatch(value):
    match value:
        case "string":
            return "string"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nested_or_patterns() {
    let source = r#"
def check_nested_or(value):
    match value:
        case [[1 | 2], [3 | 4]]:
            return "nested or"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_or_patterns_with_guards() {
    let source = r#"
def check_or_with_guard(value):
    match value:
        case 1 | 2 if value > 0:
            return "positive small"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_empty_tuple_vs_empty_list() {
    let source = r#"
def check_empty_structures(value):
    match value:
        case ():
            return "empty tuple"
        case []:
            return "empty list"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_dict_rest_patterns() {
    let source = r#"
def check_dict_rest(value):
    match value:
        case {"a": x, **rest}:
            return "has a"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_multiple_dict_patterns() {
    let source = r#"
def check_multiple_dict(value):
    match value:
        case {"x": a}:
            return "has x"
        case {"y": b}:
            return "has y"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_star_pattern_variations() {
    let source = r#"
def check_star_variations(value):
    match value:
        case []:
            return "empty"
        case [x]:
            return "one"
        case [x, y]:
            return "two"
        case [x, y, *rest]:
            return "three or more"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_star_at_beginning() {
    let source = r#"
def check_star_beginning(value):
    match value:
        case []:
            return "empty"
        case [*start, last]:
            return "has last"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_star_in_middle() {
    let source = r#"
def check_star_middle(value):
    match value:
        case []:
            return "empty"
        case [first]:
            return "single"
        case [first, *middle, last]:
            return "multiple"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_union_type_matching() {
    let source = r#"
def check_union(value):
    match value:
        case int():
            return "int"
        case str():
            return "str"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_deeply_nested_structures() {
    let source = r#"
def check_deeply_nested(value):
    match value:
        case {"a": {"b": {"c": x}}}:
            return "deep"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_mixed_sequence_types() {
    let source = r#"
def check_mixed_sequences(value):
    match value:
        case (x, y):
            return "tuple pair"
        case [x, y]:
            return "list pair"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_literal_exhaustiveness_bool() {
    let source = r#"
def check_bool_literals(value):
    match value:
        case True:
            return "true"
        case False:
            return "false"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_with_kwargs() {
    let source = r#"
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

def check_person(value):
    match value:
        case Person(name="Alice"):
            return "alice"
        case Person(name="Bob"):
            return "bob"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_redundant_after_star_pattern() {
    let source = r#"
def check_redundant_star(value):
    match value:
        case [*items]:
            return "any list"
        case []:
            return "empty"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("Unreachable")
        .assert_some();
}

#[test]
fn test_specific_before_general() {
    let source = r#"
def check_order(value):
    match value:
        case []:
            return "empty"
        case [1]:
            return "one"
        case [1, 2]:
            return "one two"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_tuple_length_variations() {
    let source = r#"
def check_tuple_lengths(value):
    match value:
        case ():
            return "empty"
        case (_,):
            return "single"
        case (_, _):
            return "pair"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_nested_class_patterns() {
    let source = r#"
class Container:
    def __init__(self, inner):
        self.inner = inner

class Inner:
    def __init__(self, value):
        self.value = value

def check_nested_class(value):
    match value:
        case Container(Inner(42)):
            return "container with 42"
        case Container(Inner(_)):
            return "container with inner"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_multiple_guards_same_pattern() {
    let source = r#"
def check_multiple_guards(value):
    match value:
        case x if x > 10:
            return "large"
        case x if x > 5:
            return "medium"
        case x if x > 0:
            return "small"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_overlapping_string_patterns() {
    let source = r#"
def check_strings(value):
    match value:
        case "hello":
            return "greeting"
        case "hello":
            return "duplicate"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("Unreachable")
        .assert_some();
}

#[test]
fn test_complex_nested_or() {
    let source = r#"
def check_complex_nested_or(value):
    match value:
        case [1 | 2, 3 | 4]:
            return "complex"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_as_pattern_in_or() {
    let source = r#"
def check_as_in_or(value):
    match value:
        case (1 as x) | (2 as x):
            return "one or two"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}

#[test]
fn test_singleton_pattern_coverage() {
    let source = r#"
def check_singletons(value):
    match value:
        case None:
            return "none"
        case True:
            return "true"
        case False:
            return "false"
        case _:
            return "other"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect_not("E8001")
        .assert_all();
}
