use coral_parser::parse;

#[test]
fn test_hir_generation() {
    let code = "x = 5 + 3";
    let result = parse(code).expect("Parse failed");

    assert!(result.owned_hir.is_some(), "HIR should be generated");

    let hir = result.owned_hir.unwrap();
    assert_eq!(hir.body.len(), 1, "Should have one statement");
}

#[test]
fn test_hir_function_definition() {
    let code = r#"def greet(name: str) -> str:
    return "Hello, " + name"#;
    let result = parse(code).expect("Parse failed");

    assert!(result.owned_hir.is_some(), "HIR should be generated");
    let hir = result.owned_hir.unwrap();
    assert_eq!(hir.body.len(), 1, "Should have function definition");
}

#[test]
fn test_hir_class_definition() {
    let code = r#"class Point:
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y"#;
    let result = parse(code).expect("Parse failed");

    assert!(result.owned_hir.is_some(), "HIR should be generated");
    let hir = result.owned_hir.unwrap();
    assert_eq!(hir.body.len(), 1, "Should have class definition");
}

#[test]
fn test_hir_with_imports() {
    let code = r#"import sys
from collections import defaultdict

x = 42"#;
    let result = parse(code).expect("Parse failed");

    assert!(result.owned_hir.is_some(), "HIR should be generated");
    let hir = result.owned_hir.unwrap();
    assert!(!hir.imports.is_empty(), "Should have imports");
}

#[test]
fn test_hir_empty_module() {
    let code = "";
    let result = parse(code).expect("Parse failed");

    assert!(
        result.owned_hir.is_some(),
        "HIR should be generated even for empty module"
    );
    let hir = result.owned_hir.unwrap();
    assert_eq!(hir.body.len(), 0, "Empty module should have no statements");
}
