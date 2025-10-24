//! Integration tests for module validation with actual file structures
//!
//! These tests create temporary file structures and test cross-module validation

use coral_parser::Arena;
use coral_parser::semantic::module::loader::ModuleLoader;
use tempfile::TempDir;

#[test]
fn test_simple_reexport_chain() {
    let temp_dir = TempDir::new().unwrap();

    std::fs::write(
        temp_dir.path().join("base.coral"),
        r#"
def add(a: int, b: int) -> int:
    return a + b

export add
"#,
    )
    .unwrap();

    std::fs::write(
        temp_dir.path().join("api.coral"),
        r#"
export add from base
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    let base_result = loader.load_module(&temp_dir.path().join("base.coral"));
    assert!(base_result.is_ok(), "base module should load successfully");

    let api_result = loader.load_module(&temp_dir.path().join("api.coral"));
    assert!(
        api_result.is_ok(),
        "api module should load successfully and validate re-export"
    );

    let registry = loader.export_registry();
    assert!(registry.is_exported("base", "add"));
}

#[test]
fn test_reexport_invalid_name() {
    let temp_dir = TempDir::new().unwrap();

    std::fs::write(
        temp_dir.path().join("source.coral"),
        r#"
def foo():
    return 42

export foo
"#,
    )
    .unwrap();

    std::fs::write(
        temp_dir.path().join("reexporter.coral"),
        r#"
export bar from source
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    let source_result = loader.load_module(&temp_dir.path().join("source.coral"));
    assert!(source_result.is_ok());

    let reexporter_result = loader.load_module(&temp_dir.path().join("reexporter.coral"));
    assert!(
        reexporter_result.is_err(),
        "Should fail when re-exporting non-existent name"
    );

    let errors = reexporter_result.unwrap_err();
    assert!(!errors.is_empty());
}

#[test]
fn test_mixed_exports_and_reexports() {
    let temp_dir = TempDir::new().unwrap();

    std::fs::write(
        temp_dir.path().join("utils.coral"),
        r#"
def helper():
    return "helper"

export helper
"#,
    )
    .unwrap();

    std::fs::write(
        temp_dir.path().join("api.coral"),
        r#"
def local_function():
    return "local"

export local_function
export helper from utils
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    let utils_result = loader.load_module(&temp_dir.path().join("utils.coral"));
    assert!(utils_result.is_ok());

    let api_result = loader.load_module(&temp_dir.path().join("api.coral"));
    assert!(api_result.is_ok());

    let registry = loader.export_registry();
    assert!(registry.is_exported("api", "local_function"));
    assert!(registry.is_exported("utils", "helper"));
}

#[test]
fn test_circular_reexport_detection() {
    let temp_dir = TempDir::new().unwrap();

    std::fs::write(
        temp_dir.path().join("circular.coral"),
        r#"
def foo():
    return 42

export foo
export foo from circular
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    let result = loader.load_module(&temp_dir.path().join("circular.coral"));
    assert!(result.is_err(), "Should detect circular re-export to self");
}

#[test]
fn test_multiple_modules_with_exports() {
    let temp_dir = TempDir::new().unwrap();

    std::fs::create_dir(temp_dir.path().join("models")).unwrap();
    std::fs::write(
        temp_dir.path().join("models").join("user.coral"),
        r#"
class User:
    def __init__(self, name: str):
        self.name = name

export User
"#,
    )
    .unwrap();

    std::fs::write(
        temp_dir.path().join("models").join("post.coral"),
        r#"
class Post:
    def __init__(self, title: str):
        self.title = title

export Post
"#,
    )
    .unwrap();

    std::fs::write(
        temp_dir.path().join("api.coral"),
        r#"
export User from models.user
export Post from models.post

def create_session():
    return "session"

export create_session
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    let user_result = loader.load_module(&temp_dir.path().join("models").join("user.coral"));
    assert!(user_result.is_ok());

    let post_result = loader.load_module(&temp_dir.path().join("models").join("post.coral"));
    assert!(post_result.is_ok());

    let api_result = loader.load_module(&temp_dir.path().join("api.coral"));
    assert!(api_result.is_ok());

    let registry = loader.export_registry();
    assert!(registry.is_exported("models.user", "User"));
    assert!(registry.is_exported("models.post", "Post"));
    assert!(registry.is_exported("api", "create_session"));
}

#[test]
fn test_duplicate_reexport_detection() {
    let temp_dir = TempDir::new().unwrap();

    std::fs::write(
        temp_dir.path().join("source.coral"),
        r#"
def func():
    return 42

export func
"#,
    )
    .unwrap();

    std::fs::write(
        temp_dir.path().join("duplicate.coral"),
        r#"
export func from source
export func from source
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    loader
        .load_module(&temp_dir.path().join("source.coral"))
        .ok();
    let result = loader.load_module(&temp_dir.path().join("duplicate.coral"));

    assert!(result.is_err(), "Should detect duplicate re-export");
}
