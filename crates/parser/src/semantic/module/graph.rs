use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use text_size::TextRange;

/// Analysis state of a module
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleState {
    /// Module has not been parsed yet
    Unparsed,
    /// Module is currently being parsed
    Parsing,
    /// Module has been parsed but not analyzed
    Parsed,
    /// Module is currently being analyzed
    Analyzing,
    /// Module analysis is complete
    Analyzed,
}

/// Information about a module in the dependency graph
#[derive(Debug)]
pub struct ModuleNode {
    /// Module name/path
    pub name: String,
    /// File path to the module source file (for diagnostics and loading)
    pub file_path: Option<PathBuf>,
    /// Current analysis state
    pub state: ModuleState,
    /// Source code span (for error reporting)
    pub span: TextRange,
    /// Modules that this module imports (dependencies)
    pub dependencies: HashSet<String>,
    /// Modules that import this module (reverse dependencies)
    pub dependents: HashSet<String>,
    /// Analysis result or error
    pub result: Option<Result<(), Vec<String>>>,
}

/// Graph representing module dependencies for parallel analysis
#[derive(Debug)]
pub struct ModuleGraph {
    /// All modules in the graph
    modules: HashMap<String, ModuleNode>,
}

impl ModuleGraph {
    /// Create a new empty module graph
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Add a module to the graph
    pub fn add_module(&mut self, name: String, span: TextRange) {
        self.add_module_with_path(name, span, None);
    }

    /// Add a module to the graph with an optional file path
    pub fn add_module_with_path(
        &mut self,
        name: String,
        span: TextRange,
        file_path: Option<PathBuf>,
    ) {
        let node = ModuleNode {
            name: name.clone(),
            file_path,
            state: ModuleState::Unparsed,
            span,
            dependencies: HashSet::new(),
            dependents: HashSet::new(),
            result: None,
        };
        self.modules.insert(name, node);
    }

    /// Register a dependency between modules
    pub fn add_dependency(&mut self, from_module: &str, to_module: &str) {
        if let Some(from_node) = self.modules.get_mut(from_module) {
            from_node.dependencies.insert(to_module.to_string());
        }

        if let Some(to_node) = self.modules.get_mut(to_module) {
            to_node.dependents.insert(from_module.to_string());
        }
    }

    /// Get a module by name
    pub fn get_module(&self, name: &str) -> Option<&ModuleNode> {
        self.modules.get(name)
    }

    /// Get a mutable reference to a module by name
    pub fn get_module_mut(&mut self, name: &str) -> Option<&mut ModuleNode> {
        self.modules.get_mut(name)
    }

    /// Update the state of a module
    pub fn set_module_state(&mut self, name: &str, state: ModuleState) {
        if let Some(module) = self.modules.get_mut(name) {
            module.state = state;
        }
    }

    /// Set the analysis result for a module
    pub fn set_module_result(&mut self, name: &str, result: Result<(), Vec<String>>) {
        if let Some(module) = self.modules.get_mut(name) {
            module.result = Some(result);
        }
    }

    /// Get all modules that can be analyzed in parallel (no unresolved dependencies)
    pub fn get_ready_modules(&self) -> Vec<String> {
        self.modules
            .iter()
            .filter(|(_, node)| {
                node.state == ModuleState::Parsed
                    && node.dependencies.iter().all(|dep| {
                        self.modules
                            .get(dep)
                            .is_some_and(|dep_node| dep_node.state == ModuleState::Analyzed)
                    })
            })
            .map(|(name, _)| name.clone())
            .collect()
    }

    /// Perform topological sort to get analysis order
    pub fn topological_sort(&self) -> Result<Vec<String>, String> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut visiting = HashSet::new();

        fn visit(
            module: &str,
            graph: &ModuleGraph,
            result: &mut Vec<String>,
            visited: &mut HashSet<String>,
            visiting: &mut HashSet<String>,
        ) -> Result<(), String> {
            if visited.contains(module) {
                return Ok(());
            }

            if visiting.contains(module) {
                return Err(format!(
                    "Circular dependency detected involving module '{}'",
                    module
                ));
            }

            visiting.insert(module.to_string());

            if let Some(node) = graph.get_module(module) {
                for dep in &node.dependencies {
                    visit(dep, graph, result, visited, visiting)?;
                }
            }

            visiting.remove(module);
            visited.insert(module.to_string());
            result.push(module.to_string());

            Ok(())
        }

        for module_name in self.modules.keys() {
            if !visited.contains(module_name) {
                visit(module_name, self, &mut result, &mut visited, &mut visiting)?;
            }
        }

        Ok(result)
    }

    /// Get all modules
    pub fn modules(&self) -> &HashMap<String, ModuleNode> {
        &self.modules
    }

    /// Get module names
    pub fn module_names(&self) -> Vec<String> {
        self.modules.keys().cloned().collect()
    }

    /// Check if all modules are in analyzed state
    pub fn is_complete(&self) -> bool {
        self.modules
            .values()
            .all(|node| node.state == ModuleState::Analyzed)
    }

    /// Get modules that have failed analysis
    pub fn get_failed_modules(&self) -> Vec<(&str, &Vec<String>)> {
        self.modules
            .iter()
            .filter_map(|(name, node)| {
                node.result.as_ref().and_then(|result| match result {
                    Err(errors) => Some((name.as_str(), errors)),
                    Ok(_) => None,
                })
            })
            .collect()
    }
}

impl Default for ModuleGraph {
    fn default() -> Self {
        Self::new()
    }
}
