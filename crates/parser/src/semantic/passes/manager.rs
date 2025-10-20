// Analysis pass manager and orchestration

use crate::ast::nodes::Module;
use crate::error::codes::Severity;
use crate::error::diagnostic::Diagnostic;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::time::{Duration, Instant};

/// Result of running an analysis pass
pub type PassResult = Result<(), Vec<Diagnostic>>;

/// Priority level for pass execution order
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PassPriority {
    /// Critical passes that must run first (name resolution, imports)
    Critical = 0,
    /// High priority passes (type inference, module system)
    High = 1,
    /// Medium priority passes (type checking, control flow)
    Medium = 2,
    /// Low priority passes (unused imports, exhaustiveness)
    Low = 3,
    /// Optional passes (performance hints, style checks)
    Optional = 4,
}

/// Metadata about an analysis pass
#[derive(Debug, Clone)]
pub struct PassMetadata {
    /// Unique identifier for the pass
    pub id: &'static str,
    /// Human-readable name
    pub name: &'static str,
    /// Description of what the pass does
    pub description: &'static str,
    /// Priority level
    pub priority: PassPriority,
    /// Pass IDs that must run before this one
    pub dependencies: Vec<&'static str>,
    /// Whether this pass can run in parallel with others
    pub parallelizable: bool,
    /// Whether this pass is enabled by default
    pub enabled_by_default: bool,
}

/// Statistics about pass execution
#[derive(Debug, Clone)]
pub struct PassStatistics {
    /// Number of times the pass has been run
    pub runs: usize,
    /// Total time spent in the pass
    pub total_duration: Duration,
    /// Average time per run
    pub average_duration: Duration,
    /// Number of errors reported
    pub errors_reported: usize,
    /// Number of warnings reported
    pub warnings_reported: usize,
}

impl PassStatistics {
    fn new() -> Self {
        PassStatistics {
            runs: 0,
            total_duration: Duration::ZERO,
            average_duration: Duration::ZERO,
            errors_reported: 0,
            warnings_reported: 0,
        }
    }

    fn record_run(&mut self, duration: Duration, errors: usize, warnings: usize) {
        self.runs += 1;
        self.total_duration += duration;
        self.average_duration = self.total_duration / self.runs as u32;
        self.errors_reported += errors;
        self.warnings_reported += warnings;
    }
}

/// Configuration for the pass manager
#[derive(Debug, Clone)]
pub struct PassManagerConfig {
    /// Whether to run passes in parallel when possible
    pub parallel_execution: bool,
    /// Whether to continue after errors
    pub continue_on_error: bool,
    /// Maximum number of errors before stopping
    pub max_errors: Option<usize>,
    /// Enable timing statistics
    pub collect_statistics: bool,
    /// Set of disabled pass IDs
    pub disabled_passes: HashSet<String>,
    /// Enable verbose output
    pub verbose: bool,
}

impl Default for PassManagerConfig {
    fn default() -> Self {
        PassManagerConfig {
            parallel_execution: false, // Default to sequential for now
            continue_on_error: true,
            max_errors: None,
            collect_statistics: false,
            disabled_passes: HashSet::new(),
            verbose: false,
        }
    }
}

/// Pass manager that orchestrates semantic analysis passes
pub struct PassManager {
    /// Configuration
    config: PassManagerConfig,
    /// Registered passes (id -> metadata)
    passes: HashMap<String, PassMetadata>,
    /// Execution order (sorted by priority and dependencies)
    execution_order: Vec<String>,
    /// Statistics for each pass
    statistics: HashMap<String, PassStatistics>,
    /// All collected diagnostics
    diagnostics: Vec<Diagnostic>,
    /// Root directory for the project
    root_dir: PathBuf,
}

impl PassManager {
    /// Create a new pass manager
    pub fn new(root_dir: PathBuf) -> Self {
        let mut manager = PassManager {
            config: PassManagerConfig::default(),
            passes: HashMap::new(),
            execution_order: Vec::new(),
            statistics: HashMap::new(),
            diagnostics: Vec::new(),
            root_dir,
        };

        // Register all available passes
        manager.register_default_passes();

        // Compute execution order
        manager.compute_execution_order();

        manager
    }

    /// Create a pass manager with custom configuration
    pub fn with_config(root_dir: PathBuf, config: PassManagerConfig) -> Self {
        let mut manager = PassManager {
            config,
            passes: HashMap::new(),
            execution_order: Vec::new(),
            statistics: HashMap::new(),
            diagnostics: Vec::new(),
            root_dir,
        };

        manager.register_default_passes();
        manager.compute_execution_order();

        manager
    }

    /// Register all default passes
    fn register_default_passes(&mut self) {
        // Critical passes (must run first)
        self.register_pass(PassMetadata {
            id: "name_resolution",
            name: "Name Resolution",
            description: "Resolves names to their definitions and checks for undefined names",
            priority: PassPriority::Critical,
            dependencies: vec![],
            parallelizable: false,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "import_resolution",
            name: "Import Resolution",
            description: "Resolves imports, detects circular dependencies, checks shadowing",
            priority: PassPriority::Critical,
            dependencies: vec![],
            parallelizable: false,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "module_system",
            name: "Module System",
            description: "Validates module structure and relationships",
            priority: PassPriority::Critical,
            dependencies: vec!["import_resolution"],
            parallelizable: false,
            enabled_by_default: true,
        });

        // High priority passes
        self.register_pass(PassMetadata {
            id: "type_inference",
            name: "Type Inference",
            description: "Infers types for expressions and variables",
            priority: PassPriority::High,
            dependencies: vec!["name_resolution"],
            parallelizable: false,
            enabled_by_default: true,
        });

        // Medium priority passes
        self.register_pass(PassMetadata {
            id: "type_checking",
            name: "Type Checking",
            description: "Validates type correctness and compatibility",
            priority: PassPriority::Medium,
            dependencies: vec!["type_inference"],
            parallelizable: false,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "control_flow",
            name: "Control Flow Analysis",
            description: "Detects unreachable code and validates return paths",
            priority: PassPriority::Medium,
            dependencies: vec!["name_resolution"],
            parallelizable: true,
            enabled_by_default: true,
        });

        // Low priority passes
        self.register_pass(PassMetadata {
            id: "exhaustiveness",
            name: "Exhaustiveness Checking",
            description: "Checks pattern matching exhaustiveness",
            priority: PassPriority::Low,
            dependencies: vec!["type_checking"],
            parallelizable: true,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "decorator_resolution",
            name: "Decorator Resolution",
            description: "Resolves and validates decorator usage",
            priority: PassPriority::Low,
            dependencies: vec!["name_resolution", "type_checking"],
            parallelizable: true,
            enabled_by_default: true,
        });

        // Optional passes (may be expensive or experimental)
        self.register_pass(PassMetadata {
            id: "ownership_check",
            name: "Ownership Checking",
            description: "Validates ownership and borrowing rules",
            priority: PassPriority::Optional,
            dependencies: vec!["type_checking"],
            parallelizable: true,
            enabled_by_default: false,
        });

        self.register_pass(PassMetadata {
            id: "concurrency_check",
            name: "Concurrency Safety",
            description: "Validates concurrent code safety",
            priority: PassPriority::Optional,
            dependencies: vec!["type_checking"],
            parallelizable: true,
            enabled_by_default: false,
        });

        self.register_pass(PassMetadata {
            id: "protocol_checking",
            name: "Protocol Checking",
            description: "Validates protocol implementations",
            priority: PassPriority::Optional,
            dependencies: vec!["type_checking"],
            parallelizable: true,
            enabled_by_default: false,
        });
    }

    /// Register a pass with the manager
    fn register_pass(&mut self, metadata: PassMetadata) {
        let id = metadata.id.to_string();
        self.statistics.insert(id.clone(), PassStatistics::new());
        self.passes.insert(id, metadata);
    }

    /// Compute the execution order based on priorities and dependencies
    fn compute_execution_order(&mut self) {
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_mark = HashSet::new();

        // Get all pass IDs sorted by priority
        let mut pass_ids: Vec<_> = self.passes.keys().cloned().collect();
        pass_ids.sort_by_key(|id| {
            self.passes
                .get(id)
                .map(|p| p.priority)
                .unwrap_or(PassPriority::Optional)
        });

        // Topological sort with dependency resolution
        for pass_id in pass_ids {
            if !visited.contains(&pass_id) {
                self.visit_pass(&pass_id, &mut visited, &mut temp_mark, &mut order);
            }
        }

        self.execution_order = order;
    }

    /// Visit a pass in topological sort (DFS)
    fn visit_pass(
        &self,
        pass_id: &str,
        visited: &mut HashSet<String>,
        temp_mark: &mut HashSet<String>,
        order: &mut Vec<String>,
    ) {
        if visited.contains(pass_id) {
            return;
        }

        if temp_mark.contains(pass_id) {
            // Circular dependency detected
            eprintln!(
                "Warning: Circular dependency detected involving pass '{}'",
                pass_id
            );
            return;
        }

        temp_mark.insert(pass_id.to_string());

        // Visit dependencies first
        if let Some(metadata) = self.passes.get(pass_id) {
            for dep in &metadata.dependencies {
                self.visit_pass(dep, visited, temp_mark, order);
            }
        }

        temp_mark.remove(pass_id);
        visited.insert(pass_id.to_string());
        order.push(pass_id.to_string());
    }

    /// Run all enabled passes on a module
    pub fn run_all_passes(&mut self, module: &Module, source: &str) -> Result<(), Vec<Diagnostic>> {
        self.diagnostics.clear();
        let mut total_errors = 0;

        for pass_id in self.execution_order.clone() {
            // Skip if disabled
            if self.config.disabled_passes.contains(&pass_id) {
                if self.config.verbose {
                    println!("Skipping disabled pass: {}", pass_id);
                }
                continue;
            }

            // Skip if not enabled by default (unless explicitly enabled)
            let metadata = self.passes.get(&pass_id).unwrap();
            if !metadata.enabled_by_default {
                if self.config.verbose {
                    println!("Skipping optional pass: {}", pass_id);
                }
                continue;
            }

            // Check max errors
            if let Some(max) = self.config.max_errors
                && total_errors >= max
            {
                if self.config.verbose {
                    println!("Stopping: reached max errors ({})", max);
                }
                break;
            }

            // Run the pass
            if self.config.verbose {
                println!("Running pass: {} ({})", metadata.name, pass_id);
            }

            let start = Instant::now();
            let result = self.run_pass(&pass_id, module, source);
            let duration = start.elapsed();

            // Collect diagnostics
            match result {
                Ok(()) => {
                    if self.config.collect_statistics
                        && let Some(stats) = self.statistics.get_mut(&pass_id)
                    {
                        stats.record_run(duration, 0, 0);
                    }
                }
                Err(diagnostics) => {
                    let error_count = diagnostics
                        .iter()
                        .filter(|d| d.severity == Severity::Error || d.severity == Severity::Fatal)
                        .count();
                    let warning_count = diagnostics
                        .iter()
                        .filter(|d| d.severity == Severity::Warning)
                        .count();

                    total_errors += error_count;

                    if self.config.collect_statistics
                        && let Some(stats) = self.statistics.get_mut(&pass_id)
                    {
                        stats.record_run(duration, error_count, warning_count);
                    }

                    self.diagnostics.extend(diagnostics);

                    if !self.config.continue_on_error && error_count > 0 {
                        if self.config.verbose {
                            println!("Stopping: pass reported {} errors", error_count);
                        }
                        break;
                    }
                }
            }

            if self.config.verbose && self.config.collect_statistics {
                println!("  Completed in {:?}", duration);
            }
        }

        if self.diagnostics.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.diagnostics))
        }
    }

    /// Run a specific pass
    fn run_pass(&self, pass_id: &str, module: &Module, source: &str) -> PassResult {
        match pass_id {
            "name_resolution" => self.run_name_resolution(module, source),
            "import_resolution" => self.run_import_resolution(module, source),
            "module_system" => self.run_module_system(module, source),
            "type_inference" => self.run_type_inference(module, source),
            "type_checking" => self.run_type_checking(module, source),
            "control_flow" => self.run_control_flow(module, source),
            "exhaustiveness" => self.run_exhaustiveness(module, source),
            "decorator_resolution" => self.run_decorator_resolution(module, source),
            "ownership_check" => self.run_ownership_check(module, source),
            "concurrency_check" => self.run_concurrency_check(module, source),
            "protocol_checking" => self.run_protocol_checking(module, source),
            _ => {
                if self.config.verbose {
                    eprintln!("Warning: Unknown pass '{}'", pass_id);
                }
                Ok(())
            }
        }
    }

    /// Run name resolution pass
    fn run_name_resolution(&self, module: &Module, _source: &str) -> PassResult {
        use crate::semantic::passes::name_resolution::NameResolver;

        let mut resolver = NameResolver::new();
        resolver.resolve_module(module);

        // Name resolution collects diagnostics internally, but we need to extract them
        // For now, return Ok(()) as the resolver doesn't expose diagnostics yet
        Ok(())
    }

    /// Run import resolution pass
    fn run_import_resolution(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::import_resolution::ImportResolver;

        // Use a placeholder file path (in real usage, this should be passed in)
        let current_file = self.root_dir.join("module.coral");
        let mut resolver = ImportResolver::new(self.root_dir.clone(), current_file);

        let _ = resolver.resolve_module(module);

        // Collect errors and warnings
        let mut diagnostics = Vec::new();

        // Add errors
        for error in resolver.errors() {
            diagnostics.push(error.to_diagnostic(source));
        }

        // Add warnings
        for warning in resolver.warnings() {
            diagnostics.push(warning.to_diagnostic(source));
        }

        if diagnostics.is_empty() {
            Ok(())
        } else {
            Err(diagnostics)
        }
    }

    /// Run control flow analysis pass
    fn run_control_flow(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::control_flow::ControlFlowAnalyzer;

        let mut analyzer = ControlFlowAnalyzer::new();
        analyzer.analyze_module(module);

        let mut diagnostics = Vec::new();

        // Collect errors
        for error in analyzer.errors() {
            diagnostics.push(error.to_diagnostic(source));
        }

        // Collect warnings
        for warning in analyzer.warnings() {
            diagnostics.push(warning.to_diagnostic(source));
        }

        if diagnostics.is_empty() {
            Ok(())
        } else {
            Err(diagnostics)
        }
    }

    /// Run module system check pass
    fn run_module_system(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::module_system::ModuleSystemChecker;
        use crate::semantic::symbol::SymbolTable;

        // Need a symbol table - create one for now
        // In production, this would be passed from name_resolution
        let symbol_table = SymbolTable::new();

        let checker = ModuleSystemChecker::new(&symbol_table);
        let errors = checker.check(module);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run type inference pass
    fn run_type_inference(&self, module: &Module, _source: &str) -> PassResult {
        use crate::semantic::passes::type_inference::{TypeInference, TypeInferenceContext};
        use crate::semantic::symbol::SymbolTable;

        // Create context
        let symbol_table = SymbolTable::new();
        let mut context = TypeInferenceContext::new(symbol_table);

        // Run inference
        let mut inferrer = TypeInference::new(&mut context);
        inferrer.infer_module(module);

        // Type inference doesn't produce errors directly
        Ok(())
    }

    /// Run type checking pass
    fn run_type_checking(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::type_checking::TypeCheckContext;
        use crate::semantic::passes::type_checking::TypeChecker;
        use crate::semantic::passes::type_inference::TypeInferenceContext;
        use crate::semantic::symbol::SymbolTable;

        // Create context (should reuse from type_inference in production)
        let symbol_table = SymbolTable::new();
        let inference_context = TypeInferenceContext::new(symbol_table);
        let mut type_context = TypeCheckContext::new(inference_context);

        let mut checker = TypeChecker::new(&mut type_context);
        checker.check_module(module);

        // Extract errors from context
        let errors = type_context.errors();
        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run exhaustiveness checking pass
    fn run_exhaustiveness(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::exhaustiveness::ExhaustivenessChecker;
        use crate::semantic::passes::type_inference::TypeInferenceContext;
        use crate::semantic::symbol::SymbolTable;

        let symbol_table = SymbolTable::new();
        let context = TypeInferenceContext::new(symbol_table);

        let mut checker = ExhaustivenessChecker::new(&context);
        let errors = checker.check_module(module);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run decorator resolution pass
    fn run_decorator_resolution(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::decorator_resolution::DecoratorResolver;
        use crate::semantic::symbol::SymbolTable;

        let symbol_table = SymbolTable::new();
        let mut resolver = DecoratorResolver::new(&symbol_table);
        let errors = resolver.check_module(module);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run ownership checking pass
    fn run_ownership_check(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::ownership_check::OwnershipChecker;

        let mut checker = OwnershipChecker::new();
        let errors = checker.check_module(module);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run concurrency safety checking pass
    fn run_concurrency_check(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::concurrency_check::ConcurrencyChecker;

        let mut checker = ConcurrencyChecker::new();
        let errors = checker.check_module(module);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run protocol checking pass
    fn run_protocol_checking(&self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::protocol_checking::ProtocolChecker;
        use crate::semantic::passes::type_inference::TypeInferenceContext;
        use crate::semantic::symbol::SymbolTable;

        let symbol_table = SymbolTable::new();
        let context = TypeInferenceContext::new(symbol_table);

        let mut checker = ProtocolChecker::new(&context);
        let errors = checker.check_module(module);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Get statistics for all passes
    pub fn get_statistics(&self) -> &HashMap<String, PassStatistics> {
        &self.statistics
    }

    /// Get statistics for a specific pass
    pub fn get_pass_statistics(&self, pass_id: &str) -> Option<&PassStatistics> {
        self.statistics.get(pass_id)
    }

    /// Get all registered passes
    pub fn get_passes(&self) -> &HashMap<String, PassMetadata> {
        &self.passes
    }

    /// Get execution order
    pub fn get_execution_order(&self) -> &[String] {
        &self.execution_order
    }

    /// Enable a pass
    pub fn enable_pass(&mut self, pass_id: &str) {
        self.config.disabled_passes.remove(pass_id);
    }

    /// Disable a pass
    pub fn disable_pass(&mut self, pass_id: &str) {
        self.config.disabled_passes.insert(pass_id.to_string());
    }

    /// Set configuration
    pub fn set_config(&mut self, config: PassManagerConfig) {
        self.config = config;
    }

    /// Get configuration
    pub fn config(&self) -> &PassManagerConfig {
        &self.config
    }

    /// Print execution order
    pub fn print_execution_order(&self) {
        println!("Analysis Pass Execution Order:");
        println!("{}", "=".repeat(50));

        for (i, pass_id) in self.execution_order.iter().enumerate() {
            if let Some(metadata) = self.passes.get(pass_id) {
                println!("{:2}. {} ({:?})", i + 1, metadata.name, metadata.priority);
                if !metadata.dependencies.is_empty() {
                    println!("    Dependencies: {:?}", metadata.dependencies);
                }
            }
        }
    }

    /// Print statistics
    pub fn print_statistics(&self) {
        if !self.config.collect_statistics {
            println!("Statistics collection is disabled");
            return;
        }

        println!("\nAnalysis Pass Statistics:");
        println!("{}", "=".repeat(80));
        println!(
            "{:<25} {:>8} {:>12} {:>12} {:>8} {:>8}",
            "Pass", "Runs", "Total Time", "Avg Time", "Errors", "Warnings"
        );
        println!("{}", "-".repeat(80));

        for pass_id in &self.execution_order {
            if let Some(stats) = self.statistics.get(pass_id)
                && stats.runs > 0
                && let Some(metadata) = self.passes.get(pass_id)
            {
                println!(
                    "{:<25} {:>8} {:>12.3?} {:>12.3?} {:>8} {:>8}",
                    metadata.name,
                    stats.runs,
                    stats.total_duration,
                    stats.average_duration,
                    stats.errors_reported,
                    stats.warnings_reported,
                );
            }
        }
        println!("{}", "=".repeat(80));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pass_manager_creation() {
        let manager = PassManager::new(PathBuf::from("/test"));
        assert!(!manager.passes.is_empty());
        assert!(!manager.execution_order.is_empty());
    }

    #[test]
    fn test_execution_order() {
        let manager = PassManager::new(PathBuf::from("/test"));

        // name_resolution and import_resolution should come before type_checking
        let order = manager.get_execution_order();
        let name_res_pos = order.iter().position(|id| id == "name_resolution");
        let type_check_pos = order.iter().position(|id| id == "type_checking");

        assert!(name_res_pos.is_some());
        assert!(type_check_pos.is_some());
        assert!(name_res_pos.unwrap() < type_check_pos.unwrap());
    }

    #[test]
    fn test_pass_dependencies() {
        let manager = PassManager::new(PathBuf::from("/test"));

        // type_checking depends on type_inference
        let order = manager.get_execution_order();
        let type_inf_pos = order.iter().position(|id| id == "type_inference");
        let type_check_pos = order.iter().position(|id| id == "type_checking");

        assert!(type_inf_pos.is_some());
        assert!(type_check_pos.is_some());
        assert!(type_inf_pos.unwrap() < type_check_pos.unwrap());
    }

    #[test]
    fn test_priority_ordering() {
        let manager = PassManager::new(PathBuf::from("/test"));

        // Critical passes should come before optional passes
        let order = manager.get_execution_order();
        let critical_max = order
            .iter()
            .enumerate()
            .filter(|(_, id)| {
                manager
                    .passes
                    .get(*id)
                    .map(|p| p.priority == PassPriority::Critical)
                    .unwrap_or(false)
            })
            .map(|(i, _)| i)
            .max()
            .unwrap_or(0);

        let optional_min = order
            .iter()
            .enumerate()
            .filter(|(_, id)| {
                manager
                    .passes
                    .get(*id)
                    .map(|p| p.priority == PassPriority::Optional)
                    .unwrap_or(false)
            })
            .map(|(i, _)| i)
            .min()
            .unwrap_or(order.len());

        assert!(
            critical_max < optional_min,
            "Critical passes should come before optional passes"
        );
    }

    #[test]
    fn test_enable_disable_pass() {
        let mut manager = PassManager::new(PathBuf::from("/test"));

        manager.disable_pass("type_checking");
        assert!(manager.config.disabled_passes.contains("type_checking"));

        manager.enable_pass("type_checking");
        assert!(!manager.config.disabled_passes.contains("type_checking"));
    }

    #[test]
    fn test_config() {
        let config = PassManagerConfig {
            parallel_execution: true,
            continue_on_error: false,
            max_errors: Some(10),
            collect_statistics: true,
            disabled_passes: HashSet::new(),
            verbose: true,
        };

        let manager = PassManager::with_config(PathBuf::from("/test"), config);
        assert!(manager.config().parallel_execution);
        assert!(!manager.config().continue_on_error);
        assert_eq!(manager.config().max_errors, Some(10));
    }

    #[test]
    fn test_pass_registration() {
        let manager = PassManager::new(PathBuf::from("/test"));

        // Verify all expected passes are registered
        assert!(manager.passes.contains_key("name_resolution"));
        assert!(manager.passes.contains_key("type_checking"));
        assert!(manager.passes.contains_key("ownership_check"));
        assert!(manager.passes.contains_key("concurrency_check"));

        // Verify pass metadata
        let ownership = manager.passes.get("ownership_check").unwrap();
        assert_eq!(ownership.priority, PassPriority::Optional);
        assert!(!ownership.enabled_by_default);
        assert!(ownership.dependencies.contains(&"type_checking"));
    }

    #[test]
    fn test_statistics_collection() {
        let config = PassManagerConfig {
            collect_statistics: true,
            ..Default::default()
        };

        let manager = PassManager::with_config(PathBuf::from("/test"), config);
        assert!(manager.config().collect_statistics);

        // Statistics should be initialized
        let stats = manager.get_statistics();
        assert!(!stats.is_empty());
    }
}
