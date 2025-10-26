//! Bytecode module structure with import/export resolution
//!
//! Represents a compiled Coral module with:
//! - All functions and classes in the module
//! - Import and export tables for module linking
//! - Global variables and constants
//! - Module metadata

use super::{ConstantPool, Function};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BytecodeModule {
    pub name: String,
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub imports: ImportTable,
    pub exports: ExportTable,
    pub constants: ConstantPool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Global {
    pub name: String,
    pub type_id: u32,
    pub initial_value_id: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportTable {
    pub entries: IndexMap<String, ImportEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportEntry {
    pub module_name: String,
    pub symbols: Vec<(String, u32)>,
    pub module_id: u16,
    pub is_native: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportTable {
    pub entries: IndexMap<String, ExportEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportEntry {
    pub name: String,
    pub type_id: u32,
    pub kind: ExportKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExportKind {
    Function,
    Class,
    Global,
}

impl BytecodeModule {
    pub fn new(name: String) -> Self {
        Self {
            name,
            functions: Vec::new(),
            globals: Vec::new(),
            imports: ImportTable::new(),
            exports: ExportTable::new(),
            constants: ConstantPool::new(),
        }
    }

    pub fn add_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    pub fn add_global(&mut self, name: String, type_id: u32, initial_value_id: Option<u32>) {
        self.globals.push(Global {
            name,
            type_id,
            initial_value_id,
        });
    }

    pub fn add_export(&mut self, name: String, type_id: u32, kind: ExportKind) {
        self.exports.entries.insert(
            name.clone(),
            ExportEntry {
                name,
                type_id,
                kind,
            },
        );
    }

    pub fn add_import(
        &mut self,
        module_name: String,
        symbols: Vec<(String, u32)>,
        module_id: u16,
        is_native: bool,
    ) {
        self.imports.entries.insert(
            module_name.clone(),
            ImportEntry {
                module_name,
                symbols,
                module_id,
                is_native,
            },
        );
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(|f| f.name == name)
    }
}

impl ImportTable {
    pub fn new() -> Self {
        Self {
            entries: IndexMap::new(),
        }
    }

    pub fn count(&self) -> usize {
        self.entries.len()
    }
}

impl Default for ImportTable {
    fn default() -> Self {
        Self::new()
    }
}

impl ExportTable {
    pub fn new() -> Self {
        Self {
            entries: IndexMap::new(),
        }
    }

    pub fn count(&self) -> usize {
        self.entries.len()
    }
}

impl Default for ExportTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_module() {
        let module = BytecodeModule::new("test_module".to_string());

        assert_eq!(module.name, "test_module");
        assert!(module.functions.is_empty());
        assert!(module.globals.is_empty());
    }

    #[test]
    fn test_add_export() {
        let mut module = BytecodeModule::new("test".to_string());
        module.add_export("my_func".to_string(), 42, ExportKind::Function);

        assert_eq!(module.exports.count(), 1);
        let entry = module.exports.entries.get("my_func").unwrap();
        assert_eq!(entry.kind, ExportKind::Function);
    }

    #[test]
    fn test_add_import() {
        let mut module = BytecodeModule::new("test".to_string());
        module.add_import(
            "other_module".to_string(),
            vec![("func".to_string(), 0)],
            1,
            false,
        );

        assert_eq!(module.imports.count(), 1);
        let entry = module.imports.entries.get("other_module").unwrap();
        assert_eq!(entry.module_id, 1);
    }
}
