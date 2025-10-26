//! Linker and Standard Library Tests
//!
//! Tests for module linking, import resolution, and standard library integration:
//! - Intrinsic function registry
//! - Native module registry
//! - Import resolution
//! - Module loading and linking

use coral_codegen::{
    ImportResolver, IntrinsicRegistry, ModuleLoader, NativeModuleRegistry,
    bytecode::{BytecodeModule, ExportEntry, ExportKind, ImportEntry, ImportTable},
};

// ===== Intrinsic Registry Tests =====

#[test]
fn test_intrinsic_registry_creation() {
    let registry = IntrinsicRegistry::new();
    assert!(registry.is_intrinsic("print"));
    assert!(registry.is_intrinsic("len"));
    assert!(registry.is_intrinsic("range"));
    assert!(registry.is_intrinsic("isinstance"));
}

#[test]
fn test_intrinsic_registry_lookup() {
    let registry = IntrinsicRegistry::new();
    assert!(registry.get("print").is_some());
    assert!(registry.get("len").is_some());
    assert!(registry.get("nonexistent").is_none());
}

#[test]
fn test_intrinsic_registry_complete_set() {
    let registry = IntrinsicRegistry::new();

    let expected_builtins = vec![
        "print",
        "len",
        "range",
        "isinstance",
        "abs",
        "min",
        "max",
        "sum",
        "reversed",
        "enumerate",
        "zip",
        "filter",
        "map",
        "sorted",
        "str",
        "int",
        "float",
        "bool",
        "list",
        "dict",
        "set",
        "tuple",
        "type",
        "callable",
        "hasattr",
        "getattr",
        "setattr",
        "delattr",
    ];

    for builtin in expected_builtins {
        assert!(
            registry.is_intrinsic(builtin),
            "Missing intrinsic: {}",
            builtin
        );
    }
}

#[test]
fn test_intrinsic_registry_enumerate() {
    let registry = IntrinsicRegistry::new();
    let count = registry.all_intrinsics().count();
    assert!(
        count >= 40,
        "Should have at least 40 intrinsics, found {}",
        count
    );
}

#[test]
fn test_intrinsic_registry_default() {
    let registry = IntrinsicRegistry::default();
    assert!(registry.is_intrinsic("print"));
}

// ===== Native Module Registry Tests =====

#[test]
fn test_native_module_registry_creation() {
    let registry = NativeModuleRegistry::new();
    assert!(registry.is_native_module("math"));
    assert!(registry.is_native_module("json"));
    assert!(registry.is_native_module("datetime"));
    assert!(registry.is_native_module("sys"));
}

#[test]
fn test_native_module_registry_get_module() {
    let registry = NativeModuleRegistry::new();
    let math = registry.get_module("math").unwrap();
    assert_eq!(math.name, "math");
    assert!(!math.functions.is_empty());
}

#[test]
fn test_native_module_registry_get_function() {
    let registry = NativeModuleRegistry::new();
    let sqrt = registry.get_function("math", "sqrt").unwrap();
    assert_eq!(sqrt.name, "sqrt");
    assert_eq!(sqrt.param_count, 1);
}

#[test]
fn test_native_module_math_functions() {
    let registry = NativeModuleRegistry::new();

    let math_functions = vec!["sqrt", "sin", "cos", "tan", "log", "exp", "floor", "ceil"];

    for func_name in math_functions {
        let func = registry.get_function("math", func_name).unwrap();
        assert_eq!(func.name, func_name);
    }
}

#[test]
fn test_native_module_json_functions() {
    let registry = NativeModuleRegistry::new();

    let json_functions = vec!["dumps", "loads"];

    for func_name in json_functions {
        let func = registry.get_function("json", func_name).unwrap();
        assert_eq!(func.name, func_name);
    }
}

#[test]
fn test_native_module_count() {
    let registry = NativeModuleRegistry::new();
    assert_eq!(registry.all_modules().count(), 9);
}

#[test]
fn test_native_module_registry_default() {
    let registry = NativeModuleRegistry::default();
    assert!(registry.is_native_module("math"));
}

// ===== Import Resolver Tests =====

#[test]
fn test_import_resolver_creation() {
    let resolver = ImportResolver::new();
    assert!(resolver.get_module_id("nonexistent").is_none());
}

#[test]
fn test_import_resolver_register_single_module() {
    let mut resolver = ImportResolver::new();
    let mut module = BytecodeModule::new("test_module".to_string());

    module.exports.entries.insert(
        "test_func".to_string(),
        ExportEntry {
            name: "test_func".to_string(),
            type_id: 1,
            kind: ExportKind::Function,
        },
    );

    let module_id = resolver.register_module(&module).unwrap();
    assert_eq!(module_id, 1);
    assert_eq!(resolver.get_module_id("test_module"), Some(1));
}

#[test]
fn test_import_resolver_multiple_modules() {
    let mut resolver = ImportResolver::new();

    let module1 = BytecodeModule::new("module1".to_string());
    let module2 = BytecodeModule::new("module2".to_string());
    let module3 = BytecodeModule::new("module3".to_string());

    let id1 = resolver.register_module(&module1).unwrap();
    let id2 = resolver.register_module(&module2).unwrap();
    let id3 = resolver.register_module(&module3).unwrap();

    assert_eq!(id1, 1);
    assert_eq!(id2, 2);
    assert_eq!(id3, 3);

    assert_eq!(resolver.get_module_id("module1"), Some(1));
    assert_eq!(resolver.get_module_id("module2"), Some(2));
    assert_eq!(resolver.get_module_id("module3"), Some(3));
}

#[test]
fn test_import_resolver_resolve_import() {
    let mut resolver = ImportResolver::new();

    let mut module = BytecodeModule::new("math".to_string());
    module.exports.entries.insert(
        "sqrt".to_string(),
        ExportEntry {
            name: "sqrt".to_string(),
            type_id: 1,
            kind: ExportKind::Function,
        },
    );

    resolver.register_module(&module).unwrap();

    let (module_id, export) = resolver.resolve_import("math", "sqrt").unwrap();
    assert_eq!(module_id, 1);
    assert_eq!(export.name, "sqrt");
}

#[test]
fn test_import_resolver_unresolved_import() {
    let resolver = ImportResolver::new();
    let result = resolver.resolve_import("nonexistent", "func");
    assert!(result.is_err());
}

#[test]
fn test_import_resolver_validate_imports() {
    let mut resolver = ImportResolver::new();

    let mut module = BytecodeModule::new("utils".to_string());
    module.exports.entries.insert(
        "helper".to_string(),
        ExportEntry {
            name: "helper".to_string(),
            type_id: 1,
            kind: ExportKind::Function,
        },
    );

    resolver.register_module(&module).unwrap();

    let mut imports = ImportTable::new();
    let entry = ImportEntry {
        module_name: "utils".to_string(),
        symbols: vec![("helper".to_string(), 1)],
        module_id: 0,
        is_native: false,
    };

    imports.entries.insert("utils".to_string(), entry);

    let result = resolver.validate_imports(&imports);
    assert!(result.is_ok());
}

#[test]
fn test_import_resolver_default() {
    let resolver = ImportResolver::default();
    assert!(resolver.get_module_id("any").is_none());
}

// ===== Module Loader Tests =====

#[test]
fn test_module_loader_creation() {
    let loader = ModuleLoader::new();
    assert_eq!(loader.module_count(), 0);
}

#[test]
fn test_module_loader_load_module() {
    let mut loader = ModuleLoader::new();
    let module = BytecodeModule::new("test".to_string());

    loader.load_module("test".to_string(), module).unwrap();
    assert_eq!(loader.module_count(), 1);
    assert!(loader.get_module("test").is_some());
}

#[test]
fn test_module_loader_load_order() {
    let mut loader = ModuleLoader::new();

    loader
        .load_module("a".to_string(), BytecodeModule::new("a".to_string()))
        .unwrap();
    loader
        .load_module("b".to_string(), BytecodeModule::new("b".to_string()))
        .unwrap();
    loader
        .load_module("c".to_string(), BytecodeModule::new("c".to_string()))
        .unwrap();

    let order = loader.load_order();
    assert_eq!(order.len(), 3);
    assert_eq!(order[0], "a");
    assert_eq!(order[1], "b");
    assert_eq!(order[2], "c");
}

#[test]
fn test_module_loader_get_module_mut() {
    let mut loader = ModuleLoader::new();
    let module = BytecodeModule::new("test".to_string());

    loader.load_module("test".to_string(), module).unwrap();

    {
        let m = loader.get_module_mut("test").unwrap();
        assert_eq!(m.name, "test");
    }

    assert!(loader.get_module("test").is_some());
}

#[test]
fn test_module_loader_enumerate_all() {
    let mut loader = ModuleLoader::new();

    loader
        .load_module("a".to_string(), BytecodeModule::new("a".to_string()))
        .unwrap();
    loader
        .load_module("b".to_string(), BytecodeModule::new("b".to_string()))
        .unwrap();

    let count = loader.all_modules().count();
    assert_eq!(count, 2);
}

#[test]
fn test_module_loader_clear_cache() {
    let mut loader = ModuleLoader::new();

    loader
        .load_module("a".to_string(), BytecodeModule::new("a".to_string()))
        .unwrap();

    assert_eq!(loader.module_count(), 1);
    loader.clear_cache();
    assert_eq!(loader.module_count(), 0);
    assert_eq!(loader.load_order().len(), 0);
}

#[test]
fn test_module_loader_circular_import_detection() {
    let mut loader = ModuleLoader::new();

    let mut module_a = BytecodeModule::new("a".to_string());
    let mut imports_a = ImportTable::new();
    imports_a.entries.insert(
        "b".to_string(),
        ImportEntry {
            module_name: "b".to_string(),
            symbols: vec![],
            module_id: 0,
            is_native: false,
        },
    );
    module_a.imports = imports_a;

    let mut module_b = BytecodeModule::new("b".to_string());
    let mut imports_b = ImportTable::new();
    imports_b.entries.insert(
        "a".to_string(),
        ImportEntry {
            module_name: "a".to_string(),
            symbols: vec![],
            module_id: 0,
            is_native: false,
        },
    );
    module_b.imports = imports_b;

    loader.load_module("a".to_string(), module_a).unwrap();
    loader.load_module("b".to_string(), module_b).unwrap();

    let result = loader.check_circular_imports();
    assert!(result.is_err(), "Should detect circular import");
}

#[test]
fn test_module_loader_default() {
    let loader = ModuleLoader::default();
    assert_eq!(loader.module_count(), 0);
}
