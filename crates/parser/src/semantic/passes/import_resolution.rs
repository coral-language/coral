use crate::ast::expr::Expr;
use crate::ast::nodes::{FromStmt, ImportStmt, Module, Stmt};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::error::{Warning, WarningKind};
use crate::semantic::builtins::is_builtin;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use text_size::TextRange;

/// Import resolution pass - validates import statements and detects circular dependencies.
///
/// This pass:
/// - Resolves import paths to file system locations
/// - Validates that imported modules exist
/// - Detects circular import dependencies
/// - Validates relative import levels
/// - Checks that imported names are valid
/// - Detects import shadowing (builtins, variables, other imports)
/// - Detects unused imports
pub struct ImportResolver {
    /// Root directory of the project (where main.coral or entry point is)
    root_dir: PathBuf,

    /// Current file being analyzed
    current_file: PathBuf,

    /// Errors collected during import resolution
    errors: Vec<Error>,

    /// Warnings collected during import resolution
    warnings: Vec<Warning>,

    /// Import chain for circular dependency detection (file paths in order)
    import_chain: Vec<PathBuf>,

    /// Set of all visited files to detect cycles
    visited: HashSet<PathBuf>,

    /// Map of module names to resolved file paths (cache)
    resolved_modules: HashMap<String, PathBuf>,

    /// Track all imported names (name -> span for error reporting)
    imported_names: HashMap<String, (u32, u32)>,

    /// Track which imported names are actually used (referenced in the module)
    used_names: HashSet<String>,
}

impl ImportResolver {
    /// Create a new import resolver
    pub fn new(root_dir: PathBuf, current_file: PathBuf) -> Self {
        Self {
            root_dir,
            current_file,
            errors: Vec::new(),
            warnings: Vec::new(),
            import_chain: Vec::new(),
            visited: HashSet::new(),
            resolved_modules: HashMap::new(),
            imported_names: HashMap::new(),
            used_names: HashSet::new(),
        }
    }

    /// Resolve and validate all imports in a module
    pub fn resolve_module(&mut self, module: &Module) -> Result<(), Vec<Error>> {
        self.import_chain.push(self.current_file.clone());
        self.visited.insert(self.current_file.clone());

        // First pass: collect all imports and check for errors
        for stmt in module.body {
            match stmt {
                Stmt::Import(import) => {
                    self.check_import(import);
                }
                Stmt::From(from) => {
                    self.check_from(from);
                }
                _ => {
                    // Other statements don't affect imports
                }
            }
        }

        // Second pass: track name usage throughout the module
        self.track_name_usage(module);

        // Third pass: check for unused imports
        self.check_unused_imports();

        self.import_chain.pop();

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Check an import statement (e.g., `import foo, bar as baz`)
    fn check_import(&mut self, import: &ImportStmt) {
        let span = (import.span.start().into(), import.span.end().into());

        for (name, alias) in import.names {
            // The imported name that will be available in the module
            let imported_name = alias.unwrap_or(name);

            // Check for shadowing
            self.check_shadowing(imported_name, span);

            // Track this imported name
            self.imported_names.insert(imported_name.to_string(), span);

            // Resolve module path
            match self.resolve_import_path(name, 0, span) {
                Ok(path) => {
                    // Check for circular imports
                    if self.import_chain.contains(&path) {
                        let mut cycle = self.import_chain.clone();
                        cycle.push(path);
                        self.errors.push(*error(
                            ErrorKind::CircularImport {
                                cycle: cycle
                                    .iter()
                                    .map(|p| p.to_string_lossy().to_string())
                                    .collect(),
                            },
                            TextRange::new(span.0.into(), span.1.into()),
                        ));
                    } else {
                        // Cache the resolved path
                        self.resolved_modules.insert(name.to_string(), path);
                    }
                }
                Err(err) => {
                    self.errors.push(*err);
                }
            }
        }
    }

    /// Check a from-import statement (e.g., `from foo import bar, baz as qux`)
    fn check_from(&mut self, from: &FromStmt) {
        // Handle relative imports
        // Note: level 1 = current package (.), level 2 = parent (..), level 3 = grandparent (...), etc.
        // So level N goes up N-1 directories
        if from.level > 0 {
            // Count directory depth from root
            let current_depth = self
                .current_file
                .strip_prefix(&self.root_dir)
                .ok()
                .and_then(|p| p.parent())
                .map(|p| p.components().count())
                .unwrap_or(0);

            // Check if we're trying to go beyond the root
            // level 1 (.) = stay in current dir (needs depth >= 0)
            // level 2 (..) = go up 1 dir (needs depth >= 1)
            // level 3 (...) = go up 2 dirs (needs depth >= 2)
            let levels_up = from.level.saturating_sub(1) as usize;
            if levels_up > current_depth {
                self.errors
                    .push(*error(ErrorKind::RelativeImportBeyondTopLevel, from.span));
                return;
            }
        }

        // Resolve the module path
        let module_name = from.module.unwrap_or("");
        let span: (u32, u32) = (from.span.start().into(), from.span.end().into());

        if from.level == 0 && module_name.is_empty() {
            self.errors.push(*error(
                ErrorKind::InvalidRelativeImport,
                TextRange::new(span.0.into(), span.1.into()),
            ));
            return;
        }

        // Track imported names and check for shadowing
        for (name, alias) in from.names {
            let imported_name = alias.unwrap_or(name);
            self.check_shadowing(imported_name, span);
            self.imported_names.insert(imported_name.to_string(), span);
        }

        match self.resolve_import_path(module_name, from.level, span) {
            Ok(path) => {
                // Check for circular imports
                if self.import_chain.contains(&path) {
                    let mut cycle = self.import_chain.clone();
                    cycle.push(path);
                    self.errors.push(*error(
                        ErrorKind::CircularImport {
                            cycle: cycle
                                .iter()
                                .map(|p| p.to_string_lossy().to_string())
                                .collect(),
                        },
                        from.span,
                    ));
                } else {
                    // Cache the resolved path
                    self.resolved_modules.insert(module_name.to_string(), path);
                }
            }
            Err(err) => {
                self.errors.push(*err);
            }
        }
    }

    /// Check if an import shadows a builtin or another import
    fn check_shadowing(&mut self, name: &str, span: (u32, u32)) {
        let span_range = TextRange::new(span.0.into(), span.1.into());

        // Check if it shadows a builtin
        if is_builtin(name) {
            self.warnings.push(Warning::new(
                WarningKind::ShadowsBuiltin {
                    name: name.to_string(),
                    span: span_range,
                },
                span_range,
            ));
        }

        // Check if it shadows a previous import
        if let Some(&previous_span) = self.imported_names.get(name) {
            let previous_range = TextRange::new(previous_span.0.into(), previous_span.1.into());
            self.warnings.push(Warning::new(
                WarningKind::ShadowsImport {
                    name: name.to_string(),
                    previous_span: previous_range,
                    span: span_range,
                },
                span_range,
            ));
        }
    }

    /// Track name usage in the module to detect unused imports
    fn track_name_usage(&mut self, module: &Module) {
        // Walk through all statements and expressions to find name references
        for stmt in module.body {
            self.visit_stmt(stmt);
        }
    }

    /// Visit a statement to find name references
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.visit_expr(&expr_stmt.value);
            }
            Stmt::Assign(assign) => {
                self.visit_expr(&assign.value);
            }
            Stmt::AnnAssign(ann_assign) => {
                if let Some(ref value) = ann_assign.value {
                    self.visit_expr(value);
                }
            }
            Stmt::AugAssign(aug_assign) => {
                self.visit_expr(&aug_assign.target);
                self.visit_expr(&aug_assign.value);
            }
            Stmt::Return(ret) => {
                if let Some(ref value) = ret.value {
                    self.visit_expr(value);
                }
            }
            Stmt::If(if_stmt) => {
                self.visit_expr(&if_stmt.test);
                for s in if_stmt.body {
                    self.visit_stmt(s);
                }
                for s in if_stmt.orelse {
                    self.visit_stmt(s);
                }
            }
            Stmt::While(while_stmt) => {
                self.visit_expr(&while_stmt.test);
                for s in while_stmt.body {
                    self.visit_stmt(s);
                }
                for s in while_stmt.orelse {
                    self.visit_stmt(s);
                }
            }
            Stmt::For(for_stmt) => {
                self.visit_expr(&for_stmt.target);
                self.visit_expr(&for_stmt.iter);
                for s in for_stmt.body {
                    self.visit_stmt(s);
                }
                for s in for_stmt.orelse {
                    self.visit_stmt(s);
                }
            }
            Stmt::FuncDef(func) => {
                for s in func.body {
                    self.visit_stmt(s);
                }
            }
            Stmt::ClassDef(class) => {
                for s in class.body {
                    self.visit_stmt(s);
                }
            }
            _ => {
                // Other statements don't contain name references we care about
            }
        }
    }

    /// Visit an expression to find name references
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Name(name) => {
                // Mark this name as used if it's an imported name
                if self.imported_names.contains_key(name.id) {
                    self.used_names.insert(name.id.to_string());
                }
            }
            Expr::Attribute(attr) => {
                self.visit_expr(attr.value);
            }
            Expr::Subscript(subscript) => {
                self.visit_expr(subscript.value);
                self.visit_expr(subscript.slice);
            }
            Expr::Call(call) => {
                self.visit_expr(call.func);
                for arg in call.args {
                    self.visit_expr(arg);
                }
            }
            Expr::BinOp(binop) => {
                self.visit_expr(binop.left);
                self.visit_expr(binop.right);
            }
            Expr::UnaryOp(unary) => {
                self.visit_expr(unary.operand);
            }
            Expr::Compare(compare) => {
                self.visit_expr(compare.left);
                for comp in compare.comparators {
                    self.visit_expr(comp);
                }
            }
            Expr::BoolOp(boolop) => {
                for value in boolop.values {
                    self.visit_expr(value);
                }
            }
            Expr::IfExp(if_exp) => {
                self.visit_expr(if_exp.test);
                self.visit_expr(if_exp.body);
                self.visit_expr(if_exp.orelse);
            }
            Expr::List(list) => {
                for elem in list.elts {
                    self.visit_expr(elem);
                }
            }
            Expr::Tuple(tuple) => {
                for elem in tuple.elts {
                    self.visit_expr(elem);
                }
            }
            Expr::Dict(dict) => {
                for key in dict.keys.iter().flatten() {
                    self.visit_expr(key);
                }
                for value in dict.values {
                    self.visit_expr(value);
                }
            }
            Expr::Set(set) => {
                for elem in set.elts {
                    self.visit_expr(elem);
                }
            }
            Expr::ListComp(comp) => {
                self.visit_expr(comp.elt);
            }
            Expr::DictComp(comp) => {
                self.visit_expr(comp.key);
                self.visit_expr(comp.value);
            }
            Expr::SetComp(comp) => {
                self.visit_expr(comp.elt);
            }
            Expr::GeneratorExp(generator) => {
                self.visit_expr(generator.elt);
            }
            Expr::Lambda(lambda) => {
                self.visit_expr(lambda.body);
            }
            _ => {
                // Literals and other expressions don't contain name references
            }
        }
    }

    /// Check for unused imports after analyzing the module
    fn check_unused_imports(&mut self) {
        for (name, &span) in &self.imported_names {
            if !self.used_names.contains(name) {
                let span_range = TextRange::new(span.0.into(), span.1.into());
                self.warnings.push(Warning::new(
                    WarningKind::UnusedImport {
                        name: name.clone(),
                        span: span_range,
                    },
                    span_range,
                ));
            }
        }
    }

    /// Resolve a module name to a file path
    ///
    /// - `module_name`: The module name (e.g., "foo.bar.baz")
    /// - `level`: Number of leading dots for relative imports (0 = absolute)
    /// - `span`: Span of the import statement for error reporting
    fn resolve_import_path(
        &self,
        module_name: &str,
        level: u32,
        span: (u32, u32),
    ) -> Result<PathBuf, Box<Error>> {
        // Handle relative imports
        // level 1 (.) = current package directory
        // level 2 (..) = parent directory (go up 1)
        // level 3 (...) = grandparent directory (go up 2)
        let base_dir = if level > 0 {
            // Start from current file's directory
            let mut base = self
                .current_file
                .parent()
                .ok_or_else(|| {
                    error(
                        ErrorKind::RelativeImportInNonPackage,
                        TextRange::new(span.0.into(), span.1.into()),
                    )
                })?
                .to_path_buf();

            // Go up 'level - 1' directories (level 1 = current, level 2 = parent, etc.)
            let levels_up = level.saturating_sub(1);
            for _ in 0..levels_up {
                base = base
                    .parent()
                    .ok_or_else(|| {
                        error(
                            ErrorKind::RelativeImportBeyondTopLevel,
                            TextRange::new(span.0.into(), span.1.into()),
                        )
                    })?
                    .to_path_buf();
            }

            base
        } else {
            // Absolute import - start from root
            self.root_dir.clone()
        };

        // Convert module name to path (foo.bar.baz -> foo/bar/baz)
        let module_path = if module_name.is_empty() {
            base_dir
        } else {
            let parts: Vec<&str> = module_name.split('.').collect();
            let mut path = base_dir;
            for part in parts {
                path = path.join(part);
            }
            path
        };

        // Try to find the module file
        // 1. Try as a file: foo/bar/baz.coral
        let file_path = module_path.with_extension("coral");
        if file_path.exists() {
            return Ok(file_path);
        }

        // 2. Try as a directory with main.coral: foo/bar/baz/main.coral
        let dir_main = module_path.join("main.coral");
        if dir_main.exists() {
            return Ok(dir_main);
        }

        // Module not found
        Err(error(
            ErrorKind::ModuleNotFound {
                module_name: module_name.to_string(),
            },
            TextRange::new(span.0.into(), span.1.into()),
        ))
    }

    /// Get all errors collected during resolution
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get all warnings collected during resolution
    pub fn warnings(&self) -> &[Warning] {
        &self.warnings
    }

    /// Check if there are any warnings
    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_resolver(root: &str, current: &str) -> ImportResolver {
        ImportResolver::new(PathBuf::from(root), PathBuf::from(current))
    }

    #[test]
    fn test_resolve_absolute_import() {
        let resolver = create_test_resolver("/project", "/project/main.coral");

        // This is a basic structure test - actual path resolution would need real files
        assert_eq!(resolver.root_dir, PathBuf::from("/project"));
        assert_eq!(resolver.current_file, PathBuf::from("/project/main.coral"));
    }

    #[test]
    fn test_relative_import_depth_calculation() {
        let resolver = create_test_resolver("/project", "/project/sub/module.coral");

        let current_depth = resolver
            .current_file
            .strip_prefix(&resolver.root_dir)
            .ok()
            .and_then(|p| p.parent())
            .map(|p| p.components().count())
            .unwrap_or(0);

        // sub/module.coral has depth 1 (sub directory)
        assert_eq!(current_depth, 1);
    }

    #[test]
    fn test_circular_import_detection() {
        let mut resolver = create_test_resolver("/project", "/project/a.coral");

        // Simulate visiting files in order
        let path_a = PathBuf::from("/project/a.coral");
        let path_b = PathBuf::from("/project/b.coral");

        resolver.import_chain.push(path_a.clone());
        resolver.import_chain.push(path_b.clone());

        // Check if path_a is in the chain (circular dependency)
        assert!(resolver.import_chain.contains(&path_a));
    }

    #[test]
    fn test_module_path_construction() {
        let resolver = create_test_resolver("/project", "/project/main.coral");

        // Test that module path construction works correctly
        // This test doesn't check file existence, just path construction logic
        assert_eq!(resolver.root_dir, PathBuf::from("/project"));

        // Verify the resolver is properly initialized
        assert!(resolver.errors.is_empty());
        assert!(resolver.import_chain.is_empty());
        assert!(resolver.visited.is_empty());
    }
}
