//! Tests for optimization features: pass-level parallelism, advanced caching, and memory optimization

use coral_parser::semantic::passes::manager::{PassManager, PassManagerConfig};
use coral_parser::semantic::symbol::table::SyncSymbolTable;
use std::path::PathBuf;

/// Test that pass-level parallelism can be enabled
#[test]
fn test_parallel_execution_config() {
    let mut config = PassManagerConfig::default();
    assert!(!config.parallel_execution); // Should be disabled by default

    config.parallel_execution = true;
    assert!(config.parallel_execution);

    let _manager = PassManager::with_config(PathBuf::from("/tmp"), config);
    // Just verify it can be created without errors
}

/// Test symbol table snapshot memory optimization
#[test]
fn test_symbol_table_snapshot_memory_optimization() {
    use coral_parser::semantic::symbol::scope::{BindingKind, Symbol};
    use text_size::TextRange;

    let sync_table = SyncSymbolTable::new();

    // Add some symbols
    sync_table.write(|table| {
        let symbol1 = Symbol::new(
            "test_var".to_string(),
            BindingKind::Assignment,
            TextRange::default(),
        );
        let symbol2 = Symbol::new(
            "test_func".to_string(),
            BindingKind::Function,
            TextRange::default(),
        );

        table.current_scope_mut().define(symbol1).unwrap();
        table.current_scope_mut().define(symbol2).unwrap();
    });

    // Create a snapshot
    let snapshot = sync_table.atomic_snapshot();

    // Verify the snapshot contains shared references
    let scope_symbols = snapshot.scope_symbols();
    assert!(!scope_symbols.is_empty());

    let module_symbols = snapshot.module_scope_symbols();
    assert!(module_symbols.contains_key("test_var"));
    assert!(module_symbols.contains_key("test_func"));

    // Verify lookup works
    let looked_up = snapshot.lookup("test_var");
    assert!(looked_up.is_some());

    // Verify copy-on-write works
    let mutable_copy = snapshot.get_symbol_mut("test_var");
    assert!(mutable_copy.is_some());
    assert_eq!(mutable_copy.unwrap().name, "test_var");
}

/// Test cache key creation
#[test]
fn test_cache_key_creation() {
    use coral_parser::semantic::passes::manager::ModuleCacheKey;
    use std::time::{SystemTime, UNIX_EPOCH};

    let mtime = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let key = ModuleCacheKey {
        module_name: "test_module".to_string(),
        mtime,
        content_hash: Some("test_hash".to_string()),
    };

    assert_eq!(key.module_name, "test_module");
    assert_eq!(key.mtime, mtime);
}
