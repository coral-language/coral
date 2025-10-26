//! Native module registry for stdlib implementations

use std::collections::HashMap;

pub struct NativeModuleRegistry {
    modules: HashMap<&'static str, Vec<&'static str>>,
}

impl NativeModuleRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            modules: HashMap::new(),
        };
        registry.register_all();
        registry
    }

    fn register_all(&mut self) {
        self.modules.insert("math", vec!["sqrt", "sin", "cos", "tan", "pi", "e"]);
        self.modules.insert("json", vec!["dumps", "loads", "dump", "load"]);
        self.modules.insert("sys", vec!["version", "platform", "exit"]);
        self.modules.insert("datetime", vec!["now", "fromtimestamp", "timedelta"]);
    }

    pub fn is_native_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    pub fn get_functions(&self, module_name: &str) -> Option<&[&'static str]> {
        self.modules.get(module_name).map(|v| v.as_slice())
    }
}

impl Default for NativeModuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}
