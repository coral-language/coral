//! Integration tests for module validation with actual file structures
//!
//! These tests create temporary file structures and test cross-module validation

use coral_parser::Arena;
use coral_parser::semantic::module::loader::ModuleLoader;
use tempfile::TempDir;

#[test]
fn test_simple_reexport_chain() {
    // Create a temporary directory structure:
    // base.coral exports `add`
    // api.coral re-exports `add` from base

    let temp_dir = TempDir::new().unwrap();

    // Create base.coral
    std::fs::write(
        temp_dir.path().join("base.coral"),
        r#"
def add(a: int, b: int) -> int:
    return a + b

export add
"#,
    )
    .unwrap();

    // Create api.coral that re-exports from base
    std::fs::write(
        temp_dir.path().join("api.coral"),
        r#"
export add from base
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    // Load modules in dependency order
    let base_result = loader.load_module(&temp_dir.path().join("base.coral"));
    assert!(base_result.is_ok(), "base module should load successfully");

    let api_result = loader.load_module(&temp_dir.path().join("api.coral"));
    assert!(
        api_result.is_ok(),
        "api module should load successfully and validate re-export"
    );

    // Verify exports are registered
    let registry = loader.export_registry();
    assert!(registry.is_exported("base", "add"));
}

#[test]
fn test_reexport_invalid_name() {
    // Test that re-exporting a non-existent name produces an error

    let temp_dir = TempDir::new().unwrap();

    // Create source.coral with only `foo` exported
    std::fs::write(
        temp_dir.path().join("source.coral"),
        r#"
def foo():
    return 42

export foo
"#,
    )
    .unwrap();

    // Create reexporter.coral trying to export `bar` which doesn't exist
    std::fs::write(
        temp_dir.path().join("reexporter.coral"),
        r#"
export bar from source
"#,
    )
    .unwrap();

    let arena = Arena::new();
    let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

    // Load source first
    let source_result = loader.load_module(&temp_dir.path().join("source.coral"));
    assert!(source_result.is_ok());

    // Load reexporter - should fail with validation error
    let reexporter_result = loader.load_module(&temp_dir.path().join("reexporter.coral"));
    assert!(
        reexporter_result.is_err(),
        "Should fail when re-exporting non-existent name"
    );

    let errors = reexporter_result.unwrap_err();
    assert!(!errors.is_empty());
    // The error should mention that 'bar' is not exported from 'source'
}

#[test]
fn test_mixed_exports_and_reexports() {
    // Test a module with both regular exports and re-exports

    let temp_dir = TempDir::new().unwrap();

    // Create utils.coral
    std::fs::write(
        temp_dir.path().join("utils.coral"),
        r#"
def helper():
    return "helper"

export helper
"#,
    )
    .unwrap();

    // Create api.coral with both local and re-exported items
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

    // Load utils first
    let utils_result = loader.load_module(&temp_dir.path().join("utils.coral"));
    assert!(utils_result.is_ok());

    // Load api - should validate both local export and re-export
    let api_result = loader.load_module(&temp_dir.path().join("api.coral"));
    assert!(api_result.is_ok());

    // Check registry has both exports
    let registry = loader.export_registry();
    assert!(registry.is_exported("api", "local_function"));
    assert!(registry.is_exported("utils", "helper"));
}

#[test]
fn test_circular_reexport_detection() {
    // Test that circular re-exports are detected

    let temp_dir = TempDir::new().unwrap();

    // Create a module that tries to re-export from itself
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
    // Test a realistic scenario with multiple modules

    let temp_dir = TempDir::new().unwrap();

    // Create models/user.coral
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

    // Create models/post.coral
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

    // Create api.coral that re-exports from both
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

    // Load all modules
    let user_result = loader.load_module(&temp_dir.path().join("models").join("user.coral"));
    assert!(user_result.is_ok());

    let post_result = loader.load_module(&temp_dir.path().join("models").join("post.coral"));
    assert!(post_result.is_ok());

    let api_result = loader.load_module(&temp_dir.path().join("api.coral"));
    assert!(api_result.is_ok());

    // Verify all exports are registered
    let registry = loader.export_registry();
    assert!(registry.is_exported("models.user", "User"));
    assert!(registry.is_exported("models.post", "Post"));
    assert!(registry.is_exported("api", "create_session"));
}

#[test]
fn test_duplicate_reexport_detection() {
    // Test that duplicate exports (including re-exports) are detected

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
