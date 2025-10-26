//! Module loader - handles module caching and lazy loading

use crate::bytecode::BytecodeModule;
use crate::error::CodegenResult;
use std::collections::HashMap;

pub struct ModuleLoader {
    cache: HashMap<String, BytecodeModule>,
    load_order: Vec<String>,
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            load_order: Vec::new(),
        }
    }

    pub fn load_module(&mut self, name: String, module: BytecodeModule) -> CodegenResult<()> {
        if !self.cache.contains_key(&name) {
            self.load_order.push(name.clone());
        }
        self.cache.insert(name, module);
        Ok(())
    }

    pub fn get_module(&self, name: &str) -> Option<&BytecodeModule> {
        self.cache.get(name)
    }

    pub fn get_module_mut(&mut self, name: &str) -> Option<&mut BytecodeModule> {
        self.cache.get_mut(name)
    }

    pub fn all_modules(&self) -> impl Iterator<Item = (&String, &BytecodeModule)> {
        self.cache.iter()
    }

    pub fn load_order(&self) -> &[String] {
        &self.load_order
    }

    pub fn module_count(&self) -> usize {
        self.cache.len()
    }

    pub fn clear_cache(&mut self) {
        self.cache.clear();
        self.load_order.clear();
    }

    pub fn check_circular_imports(&self) -> CodegenResult<()> {
        for (_, module) in self.cache.iter() {
            for (_, entry) in module.imports.entries.iter() {
                if let Some(imported) = self.cache.get(&entry.module_name) {
                    for (_, import_entry) in imported.imports.entries.iter() {
                        if import_entry.module_name == module.name {
                            return Err(format!(
                                "Circular import detected: {} ↔ {}",
                                module.name, entry.module_name
                            )
                            .into());
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_loader_new() {
        let loader = ModuleLoader::new();
        assert_eq!(loader.module_count(), 0);
    }

    #[test]
    fn test_load_module() {
        let mut loader = ModuleLoader::new();
        let module = BytecodeModule::new("test".to_string());
        loader.load_module("test".to_string(), module).unwrap();
        assert_eq!(loader.module_count(), 1);
        assert!(loader.get_module("test").is_some());
    }

    #[test]
    fn test_load_order() {
        let mut loader = ModuleLoader::new();
        loader
            .load_module("a".to_string(), BytecodeModule::new("a".to_string()))
            .unwrap();
        loader
            .load_module("b".to_string(), BytecodeModule::new("b".to_string()))
            .unwrap();
        assert_eq!(loader.load_order(), &["a".to_string(), "b".to_string()]);
    }
}
