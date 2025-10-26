use crate::ast::nodes::Module;
use crate::error::codes::Severity;
use crate::error::diagnostic::Diagnostic;
use crate::semantic::cache::{CacheConfig, PersistentCache};
use crate::semantic::metrics::{MetricsCollector, MetricsConfig};
use crate::semantic::module::{
    CompositeResolver, ModuleExportRegistry, ModuleGraph, ModulePathResolver,
};
use crate::semantic::passes::control_flow::ControlFlowGraph;
use crate::semantic::passes::type_inference::TypeInferenceContext;
use crate::semantic::symbol::table::SymbolTable;
use crate::semantic::types::Type;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

/// Shared analysis context containing state that persists across passes
///
/// This struct centralizes all mutable state that needs to be shared between
/// analysis passes, using Arc<RwLock<T>> for thread-safe access. This enables:
/// - State persistence between passes (avoiding redundant analysis)
/// - Parallel execution with proper synchronization
/// - Incremental compilation with cached results
#[derive(Clone)]
pub struct AnalysisContext {
    /// Shared symbol table with name bindings and scope information
    pub symbol_table: Arc<RwLock<SymbolTable>>,

    /// Type inference context containing inferred types for expressions
    pub type_context: Arc<RwLock<TypeInferenceContext>>,

    /// Cached control flow graphs (function name -> CFG)
    /// Eliminates redundant CFG construction across multiple passes
    pub cfg_cache: Arc<RwLock<HashMap<String, ControlFlowGraph>>>,

    /// Class metadata extracted from HIR for type checking
    /// Maps (class_name, attribute_name) -> attribute_type
    pub class_attributes: Arc<RwLock<HashMap<(String, String), Type>>>,

    /// Method resolution order (MRO) for each class
    pub class_mro: Arc<RwLock<HashMap<String, Vec<String>>>>,

    /// Module exports for cross-module type resolution
    /// Maps module_name -> (exported_name -> type)
    pub module_exports: Arc<RwLock<HashMap<String, HashMap<String, Type>>>>,

    /// Ownership analysis recommendations for code generation
    pub ownership_recommendations:
        Arc<RwLock<crate::semantic::passes::ownership_check::OwnershipRecommendations>>,
}

impl AnalysisContext {
    /// Create a new analysis context with empty state
    pub fn new() -> Self {
        Self {
            symbol_table: Arc::new(RwLock::new(SymbolTable::new())),
            type_context: Arc::new(RwLock::new(TypeInferenceContext::new(SymbolTable::new()))),
            cfg_cache: Arc::new(RwLock::new(HashMap::new())),
            class_attributes: Arc::new(RwLock::new(HashMap::new())),
            class_mro: Arc::new(RwLock::new(HashMap::new())),
            module_exports: Arc::new(RwLock::new(HashMap::new())),
            ownership_recommendations: Arc::new(RwLock::new(Default::default())),
        }
    }

    /// Clear all cached state (useful for incremental compilation)
    pub fn clear_caches(&self) {
        self.cfg_cache.write().unwrap().clear();
        self.class_attributes.write().unwrap().clear();
        self.class_mro.write().unwrap().clear();
        self.module_exports.write().unwrap().clear();
        self.ownership_recommendations
            .write()
            .unwrap()
            .function_analyses
            .clear();
    }

    /// Store class attribute information from HIR analysis
    pub fn store_class_attribute(&self, class_name: String, attr_name: String, attr_type: Type) {
        let mut attrs = self.class_attributes.write().unwrap();
        attrs.insert((class_name, attr_name), attr_type);
    }

    /// Look up a class attribute type
    pub fn get_class_attribute(&self, class_name: &str, attr_name: &str) -> Option<Type> {
        let attrs = self.class_attributes.read().unwrap();
        attrs
            .get(&(class_name.to_string(), attr_name.to_string()))
            .cloned()
    }

    /// Store class MRO (method resolution order)
    pub fn store_class_mro(&self, class_name: String, mro: Vec<String>) {
        let mut mro_map = self.class_mro.write().unwrap();
        mro_map.insert(class_name, mro);
    }

    /// Get class MRO
    pub fn get_class_mro(&self, class_name: &str) -> Option<Vec<String>> {
        let mro_map = self.class_mro.read().unwrap();
        mro_map.get(class_name).cloned()
    }

    /// Store module exports for cross-module type resolution
    pub fn store_module_export(&self, module_name: String, export_name: String, export_type: Type) {
        let mut exports = self.module_exports.write().unwrap();
        exports
            .entry(module_name)
            .or_default()
            .insert(export_name, export_type);
    }

    /// Get a module export type
    pub fn get_module_export(&self, module_name: &str, export_name: &str) -> Option<Type> {
        let exports = self.module_exports.read().unwrap();
        exports
            .get(module_name)
            .and_then(|module_exports| module_exports.get(export_name))
            .cloned()
    }

    /// Get or create a CFG for a function
    pub fn get_or_create_cfg<F>(&self, function_name: &str, create_fn: F) -> ControlFlowGraph
    where
        F: FnOnce() -> ControlFlowGraph,
    {
        {
            let cache = self.cfg_cache.read().unwrap();
            if let Some(cfg) = cache.get(function_name) {
                return (*cfg).clone();
            }
        }

        let cfg = create_fn();
        let mut cache = self.cfg_cache.write().unwrap();
        cache.insert(function_name.to_string(), cfg.clone());
        cfg
    }
}

impl Default for AnalysisContext {
    fn default() -> Self {
        Self::new()
    }
}

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

/// Cache key for module analysis results
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ModuleCacheKey {
    /// Module name
    pub module_name: String,
    /// File modification time
    pub mtime: u64,
    /// Content hash (SHA-256) for precise invalidation
    pub content_hash: Option<String>,
}

/// Cached result for a single pass
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PassCacheResult {
    /// Whether the pass succeeded
    pub success: bool,
    /// Diagnostics reported by the pass
    pub diagnostics: Vec<Diagnostic>,
    /// When this result was cached
    pub timestamp: u64,
    /// Content hash when this pass was run (for invalidation)
    pub content_hash: String,
}

/// Cached results for a module analysis
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ModuleCacheResult {
    /// Results for each pass (pass_id -> result)
    pub pass_results: HashMap<String, PassCacheResult>,
    /// Overall success (all passes succeeded)
    pub overall_success: bool,
    /// All diagnostics from all passes
    pub all_diagnostics: Vec<Diagnostic>,
    /// When the module was last analyzed
    pub analysis_timestamp: u64,
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
    /// Set of explicitly enabled pass IDs (overrides enabled_by_default)
    pub enabled_passes: HashSet<String>,
    /// Enable verbose output
    pub verbose: bool,
    /// Enable persistent caching
    pub enable_persistent_cache: bool,
    /// Configuration for persistent cache
    pub cache_config: Option<CacheConfig>,
    /// Configuration for metrics collection
    pub metrics_config: MetricsConfig,
    /// Timeout for individual pass execution (seconds)
    pub pass_timeout_seconds: Option<u64>,
    /// Maximum depth for re-export chain resolution (prevents infinite loops)
    pub max_reexport_depth: usize,
    /// Enable strict attribute checking (reject dynamic/unknown attributes)
    /// When false, allows Python-style dynamic attributes (default: true)
    pub strict_attribute_checking: bool,
    /// Per-pass severity overrides (pass_id -> minimum severity)
    /// When set, diagnostics from that pass are filtered to the specified severity
    pub pass_severity_overrides: HashMap<String, Severity>,
}

impl Default for PassManagerConfig {
    fn default() -> Self {
        PassManagerConfig {
            parallel_execution: true,
            continue_on_error: true,
            max_errors: None,
            collect_statistics: true,
            disabled_passes: HashSet::new(),
            enabled_passes: HashSet::new(),
            verbose: false,
            enable_persistent_cache: true,
            cache_config: Some(CacheConfig::default()),
            metrics_config: MetricsConfig::default(),
            pass_timeout_seconds: None,
            max_reexport_depth: 10,
            strict_attribute_checking: true,
            pass_severity_overrides: HashMap::new(),
        }
    }
}

impl PassManagerConfig {
    /// Set severity override for a specific pass
    pub fn set_pass_severity(&mut self, pass_id: impl Into<String>, severity: Severity) {
        self.pass_severity_overrides
            .insert(pass_id.into(), severity);
    }

    /// Get severity override for a specific pass
    pub fn get_pass_severity(&self, pass_id: &str) -> Option<Severity> {
        self.pass_severity_overrides.get(pass_id).copied()
    }

    /// Check if a pass has a severity override
    pub fn has_pass_severity_override(&self, pass_id: &str) -> bool {
        self.pass_severity_overrides.contains_key(pass_id)
    }

    /// Filter diagnostics for a specific pass based on configured severity overrides
    pub fn filter_diagnostics_for_pass(
        &self,
        diagnostics: Vec<Diagnostic>,
        pass_id: &str,
    ) -> Vec<Diagnostic> {
        if let Some(min_severity) = self.get_pass_severity(pass_id) {
            diagnostics
                .into_iter()
                .filter(|d| d.severity >= min_severity)
                .collect()
        } else {
            diagnostics
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
    /// Module path resolver
    resolver: Box<dyn ModulePathResolver + Send + Sync>,
    /// Module dependency graph for parallel analysis
    module_graph: Option<ModuleGraph>,
    /// Cache for module analysis results with advanced caching
    module_cache: HashMap<ModuleCacheKey, ModuleCacheResult>,
    /// Persistent cache for disk storage
    #[allow(dead_code)]
    persistent_cache: Option<PersistentCache>,
    /// Metrics collector for memory and performance tracking
    #[allow(dead_code)]
    metrics_collector: Option<MetricsCollector>,
    /// Historical execution times for load balancing (pass_id -> average duration in nanoseconds)
    pass_execution_times: HashMap<String, u128>,
    /// Shared analysis context containing all state that persists across passes
    /// This replaces the previous individual state fields (symbol_table, type_context, etc.)
    analysis_context: AnalysisContext,
    /// Export registry for cross-module validation
    export_registry: ModuleExportRegistry,
    /// Current file being analyzed (used for proper import resolution)
    current_file: Option<PathBuf>,
}

impl PassManager {
    /// Create a new pass manager
    pub fn new(root_dir: PathBuf) -> Self {
        let resolver: Box<dyn ModulePathResolver + Send + Sync> =
            Box::new(CompositeResolver::new(vec![root_dir.clone()]));
        let persistent_cache = None; // Disabled by default
        let metrics_collector = None; // Disabled by default

        let mut manager = PassManager {
            config: PassManagerConfig::default(),
            passes: HashMap::new(),
            execution_order: Vec::new(),
            statistics: HashMap::new(),
            diagnostics: Vec::new(),
            root_dir,
            resolver,
            module_graph: None,
            module_cache: HashMap::new(),
            persistent_cache,
            metrics_collector,
            pass_execution_times: HashMap::new(),
            analysis_context: AnalysisContext::new(),
            export_registry: ModuleExportRegistry::new(),
            current_file: None,
        };

        manager.register_default_passes();

        manager.compute_execution_order();

        manager
    }

    /// Create a pass manager with custom configuration
    pub fn with_config(root_dir: PathBuf, config: PassManagerConfig) -> Self {
        let resolver: Box<dyn ModulePathResolver + Send + Sync> =
            Box::new(CompositeResolver::new(vec![root_dir.clone()]));
        let persistent_cache = if config.enable_persistent_cache {
            config.cache_config.as_ref().and_then(|cache_config| {
                match PersistentCache::new(cache_config.clone()) {
                    Ok(cache) => Some(cache),
                    Err(e) => {
                        eprintln!("Warning: Failed to initialize persistent cache: {}", e);
                        None
                    }
                }
            })
        } else {
            None
        };

        let metrics_collector = Some(MetricsCollector::new(config.metrics_config.clone()));

        let mut manager = PassManager {
            config,
            passes: HashMap::new(),
            execution_order: Vec::new(),
            statistics: HashMap::new(),
            diagnostics: Vec::new(),
            root_dir,
            resolver,
            module_graph: None,
            module_cache: HashMap::new(),
            persistent_cache,
            metrics_collector,
            pass_execution_times: HashMap::new(),
            analysis_context: AnalysisContext::new(),
            export_registry: ModuleExportRegistry::new(),
            current_file: None,
        };

        manager.register_default_passes();
        manager.compute_execution_order();

        manager
    }

    /// Register all default passes
    fn register_default_passes(&mut self) {
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
            dependencies: vec!["name_resolution"],
            parallelizable: false,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "module_system",
            name: "Module System",
            description: "Validates module structure and relationships",
            priority: PassPriority::Critical,
            dependencies: vec!["name_resolution", "import_resolution"],
            parallelizable: false,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "hir_lowering",
            name: "HIR Lowering",
            description: "Transforms AST to High-Level Intermediate Representation with type information",
            priority: PassPriority::High,
            dependencies: vec!["name_resolution"],
            parallelizable: false,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "type_inference",
            name: "Type Inference",
            description: "Infers types for expressions and variables",
            priority: PassPriority::High,
            dependencies: vec!["hir_lowering"],
            parallelizable: true,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "type_checking",
            name: "Type Checking",
            description: "Validates type correctness and compatibility",
            priority: PassPriority::Medium,
            dependencies: vec!["hir_lowering", "type_inference"],
            parallelizable: true,
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

        self.register_pass(PassMetadata {
            id: "definite_assignment",
            name: "Definite Assignment Analysis",
            description: "Detects use of uninitialized variables",
            priority: PassPriority::Medium,
            dependencies: vec!["control_flow"],
            parallelizable: true,
            enabled_by_default: true,
        });

        self.register_pass(PassMetadata {
            id: "constant_propagation",
            name: "Constant Propagation",
            description: "Propagates constants and detects dead code from constant conditions",
            priority: PassPriority::Medium,
            dependencies: vec!["control_flow"],
            parallelizable: true,
            enabled_by_default: true,
        });

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
            id: "async_validation",
            name: "Async/Await Validation",
            description: "Validates async/await usage and detects blocking calls in async contexts",
            priority: PassPriority::Medium,
            dependencies: vec!["type_inference"],
            parallelizable: true,
            enabled_by_default: true,
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

        let mut pass_ids: Vec<_> = self.passes.keys().cloned().collect();
        pass_ids.sort_by_key(|id| {
            self.passes
                .get(id)
                .map(|p| p.priority)
                .unwrap_or(PassPriority::Optional)
        });

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
            eprintln!(
                "Warning: Circular dependency detected involving pass '{}'",
                pass_id
            );
            return;
        }

        temp_mark.insert(pass_id.to_string());

        if let Some(metadata) = self.passes.get(pass_id) {
            for dep in &metadata.dependencies {
                self.visit_pass(dep, visited, temp_mark, order);
            }
        }

        temp_mark.remove(pass_id);
        visited.insert(pass_id.to_string());
        order.push(pass_id.to_string());
    }

    /// Take ownership of the symbol table (for returning in ParseResult)
    pub fn take_symbol_table(&mut self) -> SymbolTable {
        let symbol_table = {
            let guard = self.analysis_context.symbol_table.read().unwrap();
            (*guard).clone()
        };

        *self.analysis_context.symbol_table.write().unwrap() = SymbolTable::new();
        symbol_table
    }

    /// Take ownership of the export registry
    pub fn take_export_registry(&mut self) -> ModuleExportRegistry {
        std::mem::take(&mut self.export_registry)
    }

    /// Take ownership of the ownership recommendations
    pub fn take_ownership_recommendations(
        &mut self,
    ) -> crate::semantic::passes::ownership_check::OwnershipRecommendations {
        let recommendations = {
            let guard = self
                .analysis_context
                .ownership_recommendations
                .read()
                .unwrap();
            (*guard).clone()
        };

        *self
            .analysis_context
            .ownership_recommendations
            .write()
            .unwrap() = Default::default();
        recommendations
    }

    /// Get the current module name from the root directory path
    fn get_current_module_name(&self) -> String {
        self.root_dir
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("__main__")
            .to_string()
    }

    /// Extract and register exports from a module
    fn register_module_exports(&mut self, module: &Module, module_name: &str) {
        use crate::ast::Stmt;
        use crate::semantic::module::exports::ExportInfo;
        use crate::semantic::types::Type;

        for stmt in module.body {
            if let Stmt::Export(export) = stmt
                && export.module.is_none()
            {
                for (name, alias) in export.names {
                    let exported_name = alias.unwrap_or(name);

                    let ty = {
                        let st = self.analysis_context.symbol_table.read().unwrap();
                        st.module_scope()
                            .lookup_local(name, |entry| entry.inferred_type.clone())
                            .flatten()
                            .unwrap_or(Type::Unknown)
                    };

                    let info = ExportInfo {
                        original_name: name.to_string(),
                        ty,
                        source_module: None,
                        reexport_chain: Vec::new(),
                        span: export.span,
                    };

                    self.export_registry.register_export(
                        module_name,
                        exported_name.to_string(),
                        info,
                    );
                }
            }
        }
    }

    /// Run all enabled passes on a module
    ///
    /// # Arguments
    /// * `module` - The AST module to analyze
    /// * `source` - The source code (for error reporting)
    /// * `current_file` - Optional path to the current file being analyzed (for import resolution)
    pub fn run_all_passes(
        &mut self,
        module: &Module,
        source: &str,
        current_file: Option<PathBuf>,
    ) -> Result<(), Vec<Diagnostic>> {
        self.diagnostics.clear();
        self.current_file = current_file;

        let mut enabled_passes = Vec::new();
        for pass_id in &self.execution_order {
            if self.config.disabled_passes.contains(pass_id) {
                if self.config.verbose {
                    println!("Skipping disabled pass: {}", pass_id);
                }
                continue;
            }

            let metadata = self.passes.get(pass_id).unwrap();
            let is_enabled =
                metadata.enabled_by_default || self.config.enabled_passes.contains(pass_id);

            if !is_enabled {
                if self.config.verbose {
                    println!("Skipping optional pass: {}", pass_id);
                }
                continue;
            }

            enabled_passes.push(pass_id.clone());
        }

        self.run_passes_sequential(&enabled_passes, module, source)?;

        if self.diagnostics.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.diagnostics))
        }
    }

    /// Run passes sequentially with full error handling
    fn run_passes_sequential(
        &mut self,
        enabled_passes: &[String],
        module: &Module,
        source: &str,
    ) -> Result<(), Vec<Diagnostic>> {
        let mut total_errors = 0;

        let content_hash = self.compute_source_hash(source);

        for pass_id in enabled_passes {
            if let Some(max) = self.config.max_errors
                && total_errors >= max
            {
                if self.config.verbose {
                    println!("Stopping: reached max errors ({})", max);
                }
                break;
            }

            let metadata = self.passes.get(pass_id).unwrap();

            if let Some(cached_result) = self.get_cached_pass_result(pass_id, &content_hash) {
                if self.config.verbose {
                    println!(
                        "Using cached result for pass: {} ({})",
                        metadata.name, pass_id
                    );
                }

                let filtered_diagnostics = self
                    .config
                    .filter_diagnostics_for_pass(cached_result.diagnostics.clone(), pass_id);
                self.diagnostics.extend(filtered_diagnostics.clone());

                if !cached_result.success {
                    let error_count = filtered_diagnostics
                        .iter()
                        .filter(|d| d.severity == Severity::Error || d.severity == Severity::Fatal)
                        .count();
                    total_errors += error_count;
                }

                continue; // Skip running the pass
            }

            if self.config.verbose {
                println!("Running pass: {} ({})", metadata.name, pass_id);
            }

            let start = Instant::now();
            let result = self.run_pass_with_timeout(
                pass_id,
                module,
                source,
                self.config.pass_timeout_seconds,
            );
            let duration = start.elapsed();

            let duration_ns = duration.as_nanos();
            let current_avg = self.pass_execution_times.get(pass_id).copied().unwrap_or(0);

            let new_avg = if current_avg == 0 {
                duration_ns
            } else {
                (duration_ns / 10) + (current_avg * 9 / 10)
            };
            self.pass_execution_times
                .insert(pass_id.to_string(), new_avg);

            let pass_success = result.is_ok();
            let pass_diagnostics = match &result {
                Ok(()) => Vec::new(),
                Err(diags) => diags.clone(),
            };

            self.store_pass_result(
                pass_id,
                PassCacheResult {
                    success: pass_success,
                    diagnostics: pass_diagnostics.clone(),
                    timestamp: std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs(),
                    content_hash: content_hash.clone(),
                },
            );

            match result {
                Ok(()) => {
                    if self.config.collect_statistics
                        && let Some(stats) = self.statistics.get_mut(pass_id)
                    {
                        stats.record_run(duration, 0, 0);
                    }
                }
                Err(diagnostics) => {
                    let filtered_diagnostics = self
                        .config
                        .filter_diagnostics_for_pass(diagnostics, pass_id);
                    let error_count = filtered_diagnostics
                        .iter()
                        .filter(|d| d.severity == Severity::Error || d.severity == Severity::Fatal)
                        .count();
                    let warning_count = filtered_diagnostics
                        .iter()
                        .filter(|d| d.severity == Severity::Warning)
                        .count();

                    total_errors += error_count;

                    if self.config.collect_statistics
                        && let Some(stats) = self.statistics.get_mut(pass_id)
                    {
                        stats.record_run(duration, error_count, warning_count);
                    }

                    self.diagnostics.extend(filtered_diagnostics);

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

        Ok(())
    }

    /// Run a specific pass
    fn run_pass(&mut self, pass_id: &str, module: &Module, source: &str) -> PassResult {
        match pass_id {
            "name_resolution" => self.run_name_resolution(module, source),
            "import_resolution" => self.run_import_resolution(module, source),
            "module_system" => self.run_module_system(module, source),
            "hir_lowering" => self.run_hir_lowering(module, source),
            "type_inference" => self.run_type_inference(module, source),
            "type_checking" => self.run_type_checking(module, source),
            "control_flow" => self.run_control_flow(module, source),
            "definite_assignment" => self.run_definite_assignment(module, source),
            "constant_propagation" => self.run_constant_propagation(module, source),
            "exhaustiveness" => self.run_exhaustiveness(module, source),
            "decorator_resolution" => self.run_decorator_resolution(module, source),
            "ownership_check" => self.run_ownership_check(module, source),
            "async_validation" => self.run_async_validation(module, source),
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
    fn run_name_resolution(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::name_resolution::NameResolver;

        let mut resolver = NameResolver::new();
        resolver.resolve_module(module);

        let (symbol_table, errors) = resolver.into_symbol_table();

        *self.analysis_context.symbol_table.write().unwrap() = symbol_table.clone();

        let mut type_ctx = self.analysis_context.type_context.write().unwrap();
        *type_ctx.symbol_table_mut() = symbol_table;
        drop(type_ctx); // Release lock

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run import resolution pass
    fn run_import_resolution(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::import_resolution::ImportResolver;

        let current_file = self
            .current_file
            .clone()
            .unwrap_or_else(|| self.root_dir.join("module.coral"));

        let mut resolver = ImportResolver::new(self.root_dir.clone(), current_file);

        let _ = resolver.resolve_module(module);

        let mut diagnostics = Vec::new();

        for error in resolver.errors() {
            diagnostics.push(error.to_diagnostic(source));
        }

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
    fn run_control_flow(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::control_flow::ControlFlowAnalyzer;

        let mut analyzer = ControlFlowAnalyzer::new();
        analyzer.analyze_module(module);

        let mut diagnostics = Vec::new();

        for error in analyzer.errors() {
            diagnostics.push(error.to_diagnostic(source));
        }

        for warning in analyzer.warnings() {
            diagnostics.push(warning.to_diagnostic(source));
        }

        {
            let mut cfg_cache = self.analysis_context.cfg_cache.write().unwrap();
            cfg_cache.clear(); // Clear old CFGs from previous module
            for (func_name, cfg) in &analyzer.function_cfgs {
                cfg_cache.insert(func_name.clone(), cfg.clone());
            }
        }

        if diagnostics.is_empty() {
            Ok(())
        } else {
            Err(diagnostics)
        }
    }

    /// Run definite assignment analysis pass
    fn run_definite_assignment(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::definite_assignment::DefiniteAssignmentPass;

        let function_cfgs = {
            let cfg_cache = self.analysis_context.cfg_cache.read().unwrap();
            cfg_cache.clone()
        };

        let mut pass = DefiniteAssignmentPass::new();
        let errors = pass.check_module_with_cfgs(module, &function_cfgs);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run constant propagation pass
    fn run_constant_propagation(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::constant_propagation::ConstantPropagationPass;

        let function_cfgs = {
            let cfg_cache = self.analysis_context.cfg_cache.read().unwrap();
            cfg_cache.clone()
        };

        let mut pass = ConstantPropagationPass::new();
        let (errors, warnings) = pass.check_module_with_cfgs(module, &function_cfgs);

        let mut diagnostics = Vec::new();
        for error in &errors {
            diagnostics.push(error.to_diagnostic(source));
        }
        for warning in &warnings {
            diagnostics.push(warning.to_diagnostic(source));
        }

        if diagnostics.is_empty() {
            Ok(())
        } else {
            Err(diagnostics)
        }
    }

    /// Run module system check pass
    fn run_module_system(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::module_system::ModuleSystemChecker;

        let symbol_table = self.analysis_context.symbol_table.read().unwrap();

        let module_name = self.get_current_module_name();

        let checker = ModuleSystemChecker::with_registry(
            &symbol_table,
            &self.export_registry,
            &module_name,
            self.config.max_reexport_depth,
        );
        let errors = checker.check(module);

        drop(symbol_table);

        self.register_module_exports(module, &module_name);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run HIR lowering pass
    fn run_hir_lowering(&mut self, module: &Module, _source: &str) -> PassResult {
        use crate::arena::Arena;
        use crate::arena::interner::Interner;
        use crate::semantic::hir::lower::HirLowerer;

        let arena = Arena::new();
        let mut interner = Interner::new();
        let mut lowerer = HirLowerer::new(&arena, &mut interner);

        match lowerer.lower_module(module) {
            Ok(_hir_module) => {
                let class_analyzer = lowerer.into_class_analyzer();

                let class_metadata_map = class_analyzer.export_metadata();

                {
                    let mut attrs = self.analysis_context.class_attributes.write().unwrap();
                    let mut mro_map = self.analysis_context.class_mro.write().unwrap();

                    for (class_name, metadata) in class_metadata_map {
                        mro_map.insert(class_name.clone(), metadata.mro.clone());

                        for (prop_name, prop_desc) in &metadata.properties {
                            let key = (class_name.clone(), prop_name.clone());
                            let attr_type = Type::AttributeDescriptor {
                                kind: crate::semantic::types::AttributeKind::Property,
                                getter_type: Box::new(prop_desc.getter_type.clone()),
                                setter_type: prop_desc
                                    .setter_type
                                    .as_ref()
                                    .map(|t| Box::new(t.clone())),
                            };
                            attrs.insert(key, attr_type);
                        }

                        for (method_name, method_type) in &metadata.methods {
                            let key = (class_name.clone(), method_name.clone());
                            attrs.entry(key).or_insert_with(|| method_type.clone());
                        }

                        for (attr_name, attr_type) in &metadata.class_attributes {
                            let key = (class_name.clone(), attr_name.clone());
                            attrs.entry(key).or_insert_with(|| attr_type.clone());
                        }

                        for (attr_name, attr_type) in &metadata.instance_attributes {
                            let key = (class_name.clone(), attr_name.clone());
                            attrs.entry(key).or_insert_with(|| attr_type.clone());
                        }

                        if let Some(constructor_type) = &metadata.constructor {
                            let key = (class_name.clone(), "constructor".to_string());
                            attrs.insert(key, constructor_type.clone());
                        }
                    }
                }

                Ok(())
            }
            Err(errors) => {
                let diagnostics: Vec<_> = errors
                    .iter()
                    .map(|e| Diagnostic::error(format!("HIR lowering error: {}", e)))
                    .collect();
                Err(diagnostics)
            }
        }
    }

    /// Run type inference pass
    fn run_type_inference(&mut self, module: &Module, _source: &str) -> PassResult {
        use crate::semantic::passes::type_inference::TypeInference;

        let mut type_context = self.analysis_context.type_context.write().unwrap();

        let module_exports = {
            let exports = self.analysis_context.module_exports.read().unwrap();
            exports.clone()
        };
        type_context.set_module_exports(module_exports);

        let class_attributes = {
            let attrs = self.analysis_context.class_attributes.read().unwrap();
            attrs.clone()
        };
        type_context.set_class_attributes(class_attributes);

        let class_mro = {
            let mro = self.analysis_context.class_mro.read().unwrap();
            mro.clone()
        };
        type_context.set_class_mro(class_mro);

        let mut inferrer = TypeInference::new(&mut type_context);
        inferrer.infer_module(module);

        Ok(())
    }

    /// Run type checking pass
    fn run_type_checking(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::type_checking::TypeCheckContext;
        use crate::semantic::passes::type_checking::TypeChecker;

        let type_ctx_guard = self.analysis_context.type_context.read().unwrap();

        let class_attributes = {
            let attrs = self.analysis_context.class_attributes.read().unwrap();
            attrs.clone()
        };

        let mut type_context = TypeCheckContext::with_class_metadata_and_config(
            &type_ctx_guard,
            class_attributes,
            self.config.strict_attribute_checking,
        );

        let mut checker = TypeChecker::new(&mut type_context);
        checker.check_module(module);

        let errors = type_context.errors();
        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run exhaustiveness checking pass
    fn run_exhaustiveness(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::exhaustiveness::ExhaustivenessChecker;

        let context = self.analysis_context.type_context.read().unwrap();

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
    fn run_decorator_resolution(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::decorator_resolution::DecoratorResolver;

        let symbol_table = self.analysis_context.symbol_table.read().unwrap();
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
    fn run_ownership_check(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::ownership_check::OwnershipChecker;

        let function_cfgs = {
            let cfg_cache = self.analysis_context.cfg_cache.read().unwrap();
            cfg_cache.clone()
        };

        let mut checker = OwnershipChecker::new();
        checker.set_cfg_cache(function_cfgs);
        let (errors, recommendations) = checker.check_module(module);

        {
            let mut recs = self
                .analysis_context
                .ownership_recommendations
                .write()
                .unwrap();
            *recs = recommendations;
        }

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run concurrency safety checking pass
    fn run_async_validation(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::async_validation::AsyncValidator;

        let type_context_ref = self.analysis_context.type_context.read().unwrap();
        let type_context = Some(&*type_context_ref);

        let cfg_cache_ref = self.analysis_context.cfg_cache.read().unwrap();

        let mut validator = AsyncValidator::new(type_context, &cfg_cache_ref);
        let errors = validator.validate_module(module);

        if errors.is_empty() {
            Ok(())
        } else {
            let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(source)).collect();
            Err(diagnostics)
        }
    }

    /// Run protocol checking pass
    fn run_protocol_checking(&mut self, module: &Module, source: &str) -> PassResult {
        use crate::semantic::passes::protocol_checking::ProtocolChecker;

        let context = self.analysis_context.type_context.read().unwrap();

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
        self.config.enabled_passes.insert(pass_id.to_string());
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

    /// Initialize module graph for parallel analysis
    pub fn init_module_graph(&mut self) -> &mut ModuleGraph {
        self.module_graph.get_or_insert_with(ModuleGraph::new)
    }

    /// Get reference to module graph
    pub fn module_graph(&self) -> Option<&ModuleGraph> {
        self.module_graph.as_ref()
    }

    /// Get mutable reference to module graph
    pub fn module_graph_mut(&mut self) -> Option<&mut ModuleGraph> {
        self.module_graph.as_mut()
    }

    /// Run parallel analysis on multiple modules
    pub fn run_parallel_analysis(
        &mut self,
        modules: &HashMap<String, (Module, String)>,
    ) -> Result<(), Vec<Diagnostic>> {
        if !self.config.parallel_execution {
            return self.run_sequential_analysis(modules);
        }

        self.build_module_dependency_graph(modules)?;

        let analysis_order = match self.module_graph().unwrap().topological_sort() {
            Ok(order) => order,
            Err(e) => {
                return Err(vec![Diagnostic::error(format!(
                    "Module dependency error: {}",
                    e
                ))]);
            }
        };

        use rayon::prelude::*;

        let results: Vec<_> = analysis_order
            .par_iter()
            .map(|module_name| {
                if let Ok(cache_key) = self.create_cache_key(module_name)
                    && let Some(cached_result) = self.module_cache.get(&cache_key)
                {
                    if cached_result.overall_success {
                        return (module_name.clone(), Ok(()));
                    } else {
                        return (
                            module_name.clone(),
                            Err(cached_result.all_diagnostics.clone()),
                        );
                    }
                }

                if let Some((module, source)) = modules.get(module_name) {
                    let mut temp_manager =
                        PassManager::with_config(self.root_dir.clone(), self.config.clone());

                    let file_path = self
                        .module_graph()
                        .and_then(|graph| graph.get_module(module_name))
                        .and_then(|node| node.file_path.clone());

                    let result = temp_manager.run_all_passes(module, source, file_path);
                    (module_name.clone(), result)
                } else {
                    (
                        module_name.clone(),
                        Err(vec![Diagnostic::error(format!(
                            "Module '{}' not found",
                            module_name
                        ))]),
                    )
                }
            })
            .collect();

        for (module_name, result) in &results {
            if let Ok(cache_key) = self.create_cache_key(module_name) {
                match result {
                    Ok(()) => {
                        let cached_result = ModuleCacheResult {
                            pass_results: HashMap::new(), // Per-pass results stored separately via store_pass_result()
                            overall_success: true,
                            all_diagnostics: Vec::new(),
                            analysis_timestamp: std::time::SystemTime::now()
                                .duration_since(std::time::UNIX_EPOCH)
                                .unwrap()
                                .as_secs(),
                        };
                        self.module_cache.insert(cache_key, cached_result);
                    }
                    Err(diagnostics) => {
                        let cached_result = ModuleCacheResult {
                            pass_results: HashMap::new(), // Per-pass results stored separately via store_pass_result()
                            overall_success: false,
                            all_diagnostics: diagnostics.clone(),
                            analysis_timestamp: std::time::SystemTime::now()
                                .duration_since(std::time::UNIX_EPOCH)
                                .unwrap()
                                .as_secs(),
                        };
                        self.module_cache.insert(cache_key, cached_result);
                    }
                }
            }
        }

        let mut all_diagnostics = Vec::new();
        for (module_name, result) in results {
            match result {
                Ok(()) => {
                    if let Some(graph) = self.module_graph_mut() {
                        graph.set_module_state(
                            &module_name,
                            crate::semantic::module::ModuleState::Analyzed,
                        );
                        graph.set_module_result(&module_name, Ok(()));
                    }
                }
                Err(diagnostics) => {
                    all_diagnostics.extend(diagnostics);
                    if let Some(graph) = self.module_graph_mut() {
                        let errors: Vec<String> = all_diagnostics
                            .iter()
                            .filter(|d| {
                                d.severity == Severity::Error || d.severity == Severity::Fatal
                            })
                            .map(|d| d.message.clone())
                            .collect();
                        graph.set_module_result(&module_name, Err(errors));
                    }
                }
            }
        }

        if all_diagnostics.is_empty() {
            Ok(())
        } else {
            Err(all_diagnostics)
        }
    }

    /// Build complete module dependency graph
    fn build_module_dependency_graph(
        &mut self,
        modules: &HashMap<String, (Module, String)>,
    ) -> Result<(), Vec<Diagnostic>> {
        let mut module_imports = HashMap::new();
        for (module_name, (module, _source)) in modules {
            let mut imports = Vec::new();
            self.extract_imports_from_module(module, &mut imports);
            module_imports.insert(module_name.clone(), imports);
        }

        let module_graph = self.init_module_graph();

        for name in module_imports.keys() {
            module_graph.add_module(name.clone(), text_size::TextRange::default());
        }

        for (module_name, imports) in module_imports {
            for import_name in imports {
                if import_name.starts_with('.') {
                    let level = import_name.chars().take_while(|&c| c == '.').count();
                    let name_part = import_name.trim_start_matches('.');

                    if !name_part.is_empty() {
                        let parts: Vec<&str> = module_name.split('.').collect();
                        if level <= parts.len() {
                            let base_parts = &parts[..parts.len().saturating_sub(level - 1)];
                            let resolved_name = if base_parts.is_empty() {
                                name_part.to_string()
                            } else {
                                format!("{}.{}", base_parts.join("."), name_part)
                            };
                            module_graph.add_dependency(&module_name, &resolved_name);
                        }
                    }
                } else {
                    module_graph.add_module(import_name.clone(), text_size::TextRange::default());
                    module_graph.add_dependency(&module_name, &import_name);
                }
            }
        }

        Ok(())
    }

    /// Clear the module analysis cache
    pub fn clear_module_cache(&mut self) {
        self.module_cache.clear();
    }

    /// Run a pass with optional timeout
    fn run_pass_with_timeout(
        &mut self,
        pass_id: &str,
        module: &Module,
        source: &str,
        timeout_seconds: Option<u64>,
    ) -> PassResult {
        if let Some(timeout_secs) = timeout_seconds {
            let start = Instant::now();
            let timeout_duration = Duration::from_secs(timeout_secs);

            let result = self.run_pass(pass_id, module, source);

            if start.elapsed() > timeout_duration {
                Err(vec![Diagnostic::error(format!(
                    "Pass '{}' exceeded timeout of {} seconds (took {:?})",
                    pass_id,
                    timeout_secs,
                    start.elapsed()
                ))])
            } else {
                result
            }
        } else {
            self.run_pass(pass_id, module, source)
        }
    }

    /// Compute SHA-256 content hash for a file
    fn compute_content_hash(&self, file_path: &Path) -> Result<String, std::io::Error> {
        use std::fs;

        let content = fs::read(file_path)?;
        let mut hasher = Sha256::new();
        hasher.update(&content);
        let hash = hasher.finalize();
        Ok(format!("{:x}", hash))
    }

    /// Compute hash of source code directly (for per-pass caching)
    fn compute_source_hash(&self, source: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(source.as_bytes());
        let hash = hasher.finalize();
        format!("{:x}", hash)
    }

    /// Get cached result for a specific pass
    fn get_cached_pass_result(&self, pass_id: &str, content_hash: &str) -> Option<PassCacheResult> {
        let current_file_key = self.current_file.as_ref()?;

        for (cache_key, cache_result) in &self.module_cache {
            let current_file_str = current_file_key.to_string_lossy();
            let file_stem_str = current_file_key.file_stem()?.to_string_lossy();
            if (cache_key.module_name == current_file_str || cache_key.module_name == file_stem_str)
                && let Some(pass_result) = cache_result.pass_results.get(pass_id)
                && pass_result.content_hash == content_hash
            {
                return Some(pass_result.clone());
            }
        }

        None
    }

    /// Store result for a specific pass in the cache
    fn store_pass_result(&mut self, pass_id: &str, result: PassCacheResult) {
        if let Some(current_file) = &self.current_file {
            let module_name = current_file.to_string_lossy().to_string();

            let cache_key = ModuleCacheKey {
                module_name: module_name.clone(),
                mtime: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs(),
                content_hash: Some(result.content_hash.clone()),
            };

            let cache_result =
                self.module_cache
                    .entry(cache_key)
                    .or_insert_with(|| ModuleCacheResult {
                        pass_results: HashMap::new(),
                        overall_success: true,
                        all_diagnostics: Vec::new(),
                        analysis_timestamp: std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap()
                            .as_secs(),
                    });

            cache_result
                .pass_results
                .insert(pass_id.to_string(), result);
        }
    }

    /// Get file modification time for a module
    fn get_module_mtime(&self, module_name: &str) -> Result<u64, std::io::Error> {
        use std::fs;

        let file_path = self
            .resolver
            .resolve_module(module_name, None, &[])
            .map_err(|_| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("Module not found: {}", module_name),
                )
            })?;

        let metadata = fs::metadata(file_path)?;
        let mtime = metadata
            .modified()?
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        Ok(mtime)
    }

    /// Create cache key for a module
    fn create_cache_key(&self, module_name: &str) -> Result<ModuleCacheKey, std::io::Error> {
        let mtime = self.get_module_mtime(module_name)?;

        let content_hash = if self.config.enable_persistent_cache {
            match self.resolver.resolve_module(module_name, None, &[]) {
                Ok(file_path) => self.compute_content_hash(&file_path).ok(),
                Err(_) => None,
            }
        } else {
            None
        };

        Ok(ModuleCacheKey {
            module_name: module_name.to_string(),
            mtime,
            content_hash,
        })
    }

    /// Invalidate cache for modules that depend on the given module
    pub fn invalidate_dependent_modules(&mut self, changed_module: &str) {
        if let Some(graph) = &self.module_graph {
            let mut to_invalidate = Vec::new();
            let mut visited = HashSet::new();

            fn find_dependents(
                current: &str,
                target: &str,
                graph: &ModuleGraph,
                visited: &mut HashSet<String>,
                result: &mut Vec<String>,
            ) {
                if visited.contains(current) {
                    return;
                }
                visited.insert(current.to_string());

                if let Some(node) = graph.get_module(current) {
                    if node.dependencies.contains(target) {
                        result.push(current.to_string());
                    }

                    for dependent in &node.dependents {
                        find_dependents(dependent, target, graph, visited, result);
                    }
                }
            }

            let mut dependents = Vec::new();
            for module_name in graph.module_names() {
                if module_name != changed_module {
                    find_dependents(
                        &module_name,
                        changed_module,
                        graph,
                        &mut visited,
                        &mut dependents,
                    );
                }
            }

            dependents.sort();
            dependents.dedup();

            for cache_key in self.module_cache.keys() {
                if dependents.contains(&cache_key.module_name) {
                    to_invalidate.push(cache_key.clone());
                }
            }

            for key in to_invalidate {
                self.module_cache.remove(&key);
            }
        }
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> (usize, usize) {
        let total_cached = self.module_cache.len();
        let successful_cached = self
            .module_cache
            .values()
            .filter(|result| result.overall_success)
            .count();
        (total_cached, successful_cached)
    }

    /// Extract import statements from a module
    fn extract_imports_from_module(&self, module: &Module, imports: &mut Vec<String>) {
        use crate::ast::nodes::Stmt;

        for stmt in module.body {
            match stmt {
                Stmt::Import(import_stmt) => {
                    for (module_name, _alias) in import_stmt.names {
                        imports.push(module_name.to_string());
                    }
                }
                Stmt::From(from_stmt) => {
                    if let Some(module_name) = from_stmt.module {
                        imports.push(module_name.to_string());
                    }
                }
                _ => {}
            }
        }
    }

    /// Check if a module name refers to a builtin module
    #[allow(dead_code)]
    fn is_builtin_module_static(name: &str) -> bool {
        matches!(
            name,
            "sys" | "os" | "math" | "json" | "re" | "datetime" | "collections" | "itertools"
        )
    }

    /// Run sequential analysis on multiple modules (fallback)
    fn run_sequential_analysis(
        &mut self,
        modules: &HashMap<String, (Module, String)>,
    ) -> Result<(), Vec<Diagnostic>> {
        let mut all_diagnostics = Vec::new();

        for (module_name, (module, source)) in modules {
            let file_path = self
                .module_graph()
                .and_then(|graph| graph.get_module(module_name))
                .and_then(|node| node.file_path.clone());

            match self.run_all_passes(module, source, file_path) {
                Ok(()) => {
                    if let Some(graph) = self.module_graph_mut() {
                        graph.set_module_state(
                            module_name,
                            crate::semantic::module::ModuleState::Analyzed,
                        );
                        graph.set_module_result(module_name, Ok(()));
                    }
                }
                Err(diagnostics) => {
                    all_diagnostics.extend(diagnostics);
                    if !self.config.continue_on_error {
                        break;
                    }
                }
            }
        }

        if all_diagnostics.is_empty() {
            Ok(())
        } else {
            Err(all_diagnostics)
        }
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
            enable_persistent_cache: false,
            cache_config: None,
            max_reexport_depth: 10,
            metrics_config: MetricsConfig::default(),
            pass_timeout_seconds: Some(30),
            disabled_passes: HashSet::new(),
            enabled_passes: HashSet::new(),
            verbose: true,
            strict_attribute_checking: false,
            pass_severity_overrides: HashMap::new(),
        };

        let manager = PassManager::with_config(PathBuf::from("/test"), config);
        assert!(manager.config().parallel_execution);
        assert!(!manager.config().continue_on_error);
        assert_eq!(manager.config().max_errors, Some(10));
    }

    #[test]
    fn test_pass_registration() {
        let manager = PassManager::new(PathBuf::from("/test"));

        assert!(manager.passes.contains_key("name_resolution"));
        assert!(manager.passes.contains_key("type_checking"));
        assert!(manager.passes.contains_key("ownership_check"));
        assert!(manager.passes.contains_key("async_validation"));

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

        let stats = manager.get_statistics();
        assert!(!stats.is_empty());
    }
}
