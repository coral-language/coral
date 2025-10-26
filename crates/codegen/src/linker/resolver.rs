//! Import resolution - validates imports and links to exports

use crate::bytecode::{BytecodeModule, ExportEntry, ImportTable};
use crate::error::{CodegenError, CodegenResult};
use std::collections::HashMap;

pub struct ImportResolver {
    exported_symbols: HashMap<String, ExportEntry>,
    module_map: HashMap<String, u32>,
    next_module_id: u32,
}

impl ImportResolver {
    pub fn new() -> Self {
        Self {
            exported_symbols: HashMap::new(),
            module_map: HashMap::new(),
            next_module_id: 1,
        }
    }

    pub fn register_module(&mut self, module: &BytecodeModule) -> CodegenResult<u32> {
        let module_id = self.next_module_id;
        self.next_module_id += 1;

        for export in module.exports.entries.values() {
            let key = format!("{}::{}", module.name, export.name);
            self.exported_symbols.insert(key, export.clone());
        }

        self.module_map.insert(module.name.clone(), module_id);
        Ok(module_id)
    }

    pub fn resolve_import(
        &self,
        module_name: &str,
        symbol_name: &str,
    ) -> CodegenResult<(u32, ExportEntry)> {
        let key = format!("{}::{}", module_name, symbol_name);
        let export = self.exported_symbols.get(&key).cloned().ok_or_else(|| {
            CodegenError::UnresolvedImport {
                module: module_name.to_string(),
                symbol: symbol_name.to_string(),
            }
        })?;

        let module_id = self.module_map.get(module_name).copied().ok_or_else(|| {
            CodegenError::ModuleError(format!("Module not found: {}", module_name))
        })?;

        Ok((module_id, export))
    }

    pub fn validate_imports(&self, imports: &ImportTable) -> CodegenResult<()> {
        for (_, entry) in imports.entries.iter() {
            for (symbol_name, _) in &entry.symbols {
                self.resolve_import(&entry.module_name, symbol_name)?;
            }
        }
        Ok(())
    }

    pub fn get_module_id(&self, module_name: &str) -> Option<u32> {
        self.module_map.get(module_name).copied()
    }

    pub fn all_modules(&self) -> impl Iterator<Item = (&String, &u32)> {
        self.module_map.iter()
    }
}

impl Default for ImportResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolver_new() {
        let resolver = ImportResolver::new();
        assert_eq!(resolver.next_module_id, 1);
    }

    #[test]
    fn test_get_module_id() {
        let mut resolver = ImportResolver::new();
        resolver.module_map.insert("test".to_string(), 42);
        assert_eq!(resolver.get_module_id("test"), Some(42));
        assert_eq!(resolver.get_module_id("nonexistent"), None);
    }
}
