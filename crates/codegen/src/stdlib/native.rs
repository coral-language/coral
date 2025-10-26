//! Native module registry - bridges to coral_stdlib Rust implementations

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NativeModuleId {
    Math,
    Json,
    Datetime,
    Sys,
    Os,
    Io,
    Time,
    Random,
    Itertools,
}

#[derive(Debug, Clone)]
pub struct NativeFunctionSignature {
    pub name: &'static str,
    pub param_count: u32,
    pub variadic: bool,
}

#[derive(Debug, Clone)]
pub struct NativeModule {
    pub id: NativeModuleId,
    pub name: &'static str,
    pub functions: Vec<NativeFunctionSignature>,
}

pub struct NativeModuleRegistry {
    modules: HashMap<&'static str, NativeModule>,
}

impl NativeModuleRegistry {
    pub fn new() -> Self {
        let mut modules = HashMap::new();

        modules.insert("math", math_module());
        modules.insert("json", json_module());
        modules.insert("datetime", datetime_module());
        modules.insert("sys", sys_module());
        modules.insert("os", os_module());
        modules.insert("io", io_module());
        modules.insert("time", time_module());
        modules.insert("random", random_module());
        modules.insert("itertools", itertools_module());

        Self { modules }
    }

    pub fn get_module(&self, name: &str) -> Option<&NativeModule> {
        self.modules.get(name)
    }

    pub fn is_native_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    pub fn all_modules(&self) -> impl Iterator<Item = &NativeModule> {
        self.modules.values()
    }

    pub fn get_function(
        &self,
        module_name: &str,
        func_name: &str,
    ) -> Option<&NativeFunctionSignature> {
        self.modules
            .get(module_name)
            .and_then(|m| m.functions.iter().find(|f| f.name == func_name))
    }
}

impl Default for NativeModuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}

fn math_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Math,
        name: "math",
        functions: vec![
            NativeFunctionSignature {
                name: "sqrt",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "sin",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "cos",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "tan",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "log",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "exp",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "floor",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "ceil",
                param_count: 1,
                variadic: false,
            },
        ],
    }
}

fn json_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Json,
        name: "json",
        functions: vec![
            NativeFunctionSignature {
                name: "dumps",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "loads",
                param_count: 1,
                variadic: false,
            },
        ],
    }
}

fn datetime_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Datetime,
        name: "datetime",
        functions: vec![
            NativeFunctionSignature {
                name: "now",
                param_count: 0,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "fromtimestamp",
                param_count: 1,
                variadic: false,
            },
        ],
    }
}

fn sys_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Sys,
        name: "sys",
        functions: vec![
            NativeFunctionSignature {
                name: "exit",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "argv",
                param_count: 0,
                variadic: false,
            },
        ],
    }
}

fn os_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Os,
        name: "os",
        functions: vec![
            NativeFunctionSignature {
                name: "getcwd",
                param_count: 0,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "chdir",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "listdir",
                param_count: 1,
                variadic: false,
            },
        ],
    }
}

fn io_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Io,
        name: "io",
        functions: vec![
            NativeFunctionSignature {
                name: "open",
                param_count: 2,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "read",
                param_count: 1,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "write",
                param_count: 2,
                variadic: false,
            },
        ],
    }
}

fn time_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Time,
        name: "time",
        functions: vec![
            NativeFunctionSignature {
                name: "time",
                param_count: 0,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "sleep",
                param_count: 1,
                variadic: false,
            },
        ],
    }
}

fn random_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Random,
        name: "random",
        functions: vec![
            NativeFunctionSignature {
                name: "random",
                param_count: 0,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "randint",
                param_count: 2,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "choice",
                param_count: 1,
                variadic: false,
            },
        ],
    }
}

fn itertools_module() -> NativeModule {
    NativeModule {
        id: NativeModuleId::Itertools,
        name: "itertools",
        functions: vec![
            NativeFunctionSignature {
                name: "chain",
                param_count: 0,
                variadic: true,
            },
            NativeFunctionSignature {
                name: "combinations",
                param_count: 2,
                variadic: false,
            },
            NativeFunctionSignature {
                name: "permutations",
                param_count: 2,
                variadic: false,
            },
        ],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_has_common_modules() {
        let registry = NativeModuleRegistry::new();
        assert!(registry.is_native_module("math"));
        assert!(registry.is_native_module("json"));
        assert!(registry.is_native_module("sys"));
    }

    #[test]
    fn test_get_module() {
        let registry = NativeModuleRegistry::new();
        let math = registry.get_module("math").unwrap();
        assert_eq!(math.name, "math");
    }

    #[test]
    fn test_get_function() {
        let registry = NativeModuleRegistry::new();
        let sqrt = registry.get_function("math", "sqrt").unwrap();
        assert_eq!(sqrt.name, "sqrt");
        assert_eq!(sqrt.param_count, 1);
    }

    #[test]
    fn test_module_count() {
        let registry = NativeModuleRegistry::new();
        assert_eq!(registry.all_modules().count(), 9);
    }
}
