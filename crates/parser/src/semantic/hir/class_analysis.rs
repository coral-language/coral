//! Class hierarchy analysis and MRO computation

use super::typed_expr::TypedExpr;
use super::typed_stmt::TypedStmt;
use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use std::collections::{HashMap, HashSet};
use text_size::TextRange;

/// Class hierarchy analyzer that computes MRO and attribute tables
#[derive(Default)]
pub struct ClassAnalyzer<'a> {
    /// All classes in the module
    classes: Vec<&'a TypedClassDefStmt<'a>>,
    /// Inheritance graph (class -> direct bases)
    inheritance_graph: HashMap<Symbol, Vec<Symbol>>,
    /// Computed MRO for each class
    mro_cache: HashMap<Symbol, Vec<Symbol>>,
    /// Attribute tables for each class
    attribute_tables: HashMap<Symbol, Vec<(Symbol, Type)>>,
    /// Method tables for each class
    method_tables: HashMap<Symbol, Vec<(Symbol, Type)>>,
}

/// Typed class definition (temporary structure for analysis)
#[derive(Debug, Clone)]
pub struct TypedClassDefStmt<'a> {
    pub name: Symbol,
    pub type_params: &'a [super::typed_stmt::TypedTypeParam<'a>],
    pub bases: &'a [TypedExpr<'a>],
    pub keywords: &'a [super::typed_expr::TypedKeyword<'a>],
    pub body: &'a [TypedStmt<'a>],
    pub decorators: &'a [TypedExpr<'a>],
    pub span: TextRange,
    pub docstring: Option<&'a str>,
}

impl<'a> ClassAnalyzer<'a> {
    /// Create a new class analyzer
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a class to the analyzer
    pub fn add_class(&mut self, class: &'a TypedClassDefStmt<'a>) {
        self.classes.push(class);

        // Extract base class names from base expressions
        let mut bases = Vec::new();
        for base in class.bases {
            if let TypedExpr::Name(name_expr) = base {
                bases.push(name_expr.symbol);
            }
        }

        self.inheritance_graph.insert(class.name, bases);
    }

    /// Analyze all classes and compute MRO and attribute tables
    pub fn analyze(&mut self) -> Result<(), ClassAnalysisError> {
        // Check for circular inheritance
        self.check_circular_inheritance()?;

        // Compute MRO for all classes
        let class_names: Vec<Symbol> = self.classes.iter().map(|c| c.name).collect();
        for class_name in class_names {
            self.compute_mro(class_name)?;
        }

        // Build attribute and method tables
        let class_refs: Vec<&TypedClassDefStmt<'a>> = self.classes.to_vec();
        for class in class_refs {
            self.build_attribute_table(class);
            self.build_method_table(class);
        }

        Ok(())
    }

    /// Get the MRO for a class
    pub fn get_mro(&self, class_name: Symbol) -> Option<&[Symbol]> {
        self.mro_cache.get(&class_name).map(|v| v.as_slice())
    }

    /// Get the attribute table for a class
    pub fn get_attributes(&self, class_name: Symbol) -> Option<&[(Symbol, Type)]> {
        self.attribute_tables.get(&class_name).map(|v| v.as_slice())
    }

    /// Get the method table for a class
    pub fn get_methods(&self, class_name: Symbol) -> Option<&[(Symbol, Type)]> {
        self.method_tables.get(&class_name).map(|v| v.as_slice())
    }

    /// Check for circular inheritance
    fn check_circular_inheritance(&self) -> Result<(), ClassAnalysisError> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for class in &self.classes {
            if !visited.contains(&class.name) {
                self.dfs_circular_check(class.name, &mut visited, &mut rec_stack)?;
            }
        }

        Ok(())
    }

    /// DFS to check for circular inheritance
    fn dfs_circular_check(
        &self,
        class_name: Symbol,
        visited: &mut HashSet<Symbol>,
        rec_stack: &mut HashSet<Symbol>,
    ) -> Result<(), ClassAnalysisError> {
        visited.insert(class_name);
        rec_stack.insert(class_name);

        if let Some(bases) = self.inheritance_graph.get(&class_name) {
            for &base in bases {
                if rec_stack.contains(&base) {
                    return Err(ClassAnalysisError::CircularInheritance {
                        class: class_name,
                        base,
                    });
                }

                if !visited.contains(&base) {
                    self.dfs_circular_check(base, visited, rec_stack)?;
                }
            }
        }

        rec_stack.remove(&class_name);
        Ok(())
    }

    /// Compute MRO using C3 linearization algorithm
    fn compute_mro(&mut self, class_name: Symbol) -> Result<Vec<Symbol>, ClassAnalysisError> {
        if let Some(cached) = self.mro_cache.get(&class_name) {
            return Ok(cached.clone());
        }

        let bases = self
            .inheritance_graph
            .get(&class_name)
            .cloned()
            .unwrap_or_default();

        if bases.is_empty() {
            // No bases, MRO is just the class itself
            let mro = vec![class_name];
            self.mro_cache.insert(class_name, mro.clone());
            return Ok(mro);
        }

        // C3 linearization algorithm
        let mut mro = vec![class_name];

        // Get MROs for all bases
        let mut base_mros = Vec::new();
        for base in &bases {
            let base_mro = self.compute_mro(*base)?;
            base_mros.push(base_mro);
        }

        // Merge MROs using C3 algorithm
        while !base_mros.is_empty() {
            // Find a candidate (first element of some MRO that doesn't appear in any other MRO's tail)
            let mut candidate = None;

            for (i, mro) in base_mros.iter().enumerate() {
                if let Some(&first) = mro.first() {
                    // Check if this candidate appears in any other MRO's tail
                    let mut is_valid = true;
                    for (j, other_mro) in base_mros.iter().enumerate() {
                        if i != j && other_mro.len() > 1 {
                            // Check if first appears in the tail of other_mro
                            for &item in other_mro.iter().skip(1) {
                                if item == first {
                                    is_valid = false;
                                    break;
                                }
                            }
                        }
                    }

                    if is_valid {
                        candidate = Some((i, first));
                        break;
                    }
                }
            }

            match candidate {
                Some((_i, candidate_class)) => {
                    mro.push(candidate_class);

                    // Remove the candidate from all MROs
                    for mro in &mut base_mros {
                        if let Some(pos) = mro.iter().position(|&x| x == candidate_class) {
                            mro.remove(pos);
                        }
                    }

                    // Remove empty MROs
                    base_mros.retain(|mro| !mro.is_empty());
                }
                None => {
                    return Err(ClassAnalysisError::MroConflict {
                        class: class_name,
                        bases,
                    });
                }
            }
        }

        self.mro_cache.insert(class_name, mro.clone());
        Ok(mro)
    }

    /// Build attribute table for a class
    fn build_attribute_table(&mut self, class: &TypedClassDefStmt<'a>) {
        let mut attributes = Vec::new();

        // Walk through the class body to find attribute assignments
        self.collect_attributes_from_body(class.body, &mut attributes);

        // Add attributes from base classes (following MRO)
        if let Some(mro) = self.mro_cache.get(&class.name) {
            for &base_class in mro.iter().skip(1) {
                // Skip the class itself
                if let Some(base_attributes) = self.attribute_tables.get(&base_class) {
                    for (name, ty) in base_attributes.iter() {
                        // Only add if not already defined
                        if !attributes.iter().any(|(n, _)| *n == *name) {
                            attributes.push((*name, ty.clone()));
                        }
                    }
                }
            }
        }

        self.attribute_tables.insert(class.name, attributes);
    }

    /// Build method table for a class
    fn build_method_table(&mut self, class: &TypedClassDefStmt<'a>) {
        let mut methods = Vec::new();

        // Walk through the class body to find method definitions
        self.collect_methods_from_body(class.body, &mut methods);

        // Add methods from base classes (following MRO)
        if let Some(mro) = self.mro_cache.get(&class.name) {
            for &base_class in mro.iter().skip(1) {
                // Skip the class itself
                if let Some(base_methods) = self.method_tables.get(&base_class) {
                    for (name, ty) in base_methods.iter() {
                        // Only add if not already defined
                        if !methods.iter().any(|(n, _)| *n == *name) {
                            methods.push((*name, ty.clone()));
                        }
                    }
                }
            }
        }

        self.method_tables.insert(class.name, methods);
    }

    /// Collect attributes from class body
    fn collect_attributes_from_body(
        &self,
        body: &[TypedStmt<'a>],
        attributes: &mut Vec<(Symbol, Type)>,
    ) {
        for stmt in body {
            match stmt {
                TypedStmt::Assign(assign) => {
                    // Simple attribute assignment: self.attr = value
                    for target in assign.targets {
                        if let TypedExpr::Attribute(attr) = target
                            && let TypedExpr::Name(_name) = attr.value
                        {
                            // Check if this is a self attribute assignment
                            if let TypedExpr::Name(_name) = &attr.value {
                                // Note: We can't easily check the symbol name here without access to the interner
                                // For now, we'll collect all attribute assignments
                                attributes.push((attr.attr, assign.value.ty().clone()));
                            }
                        }
                    }
                }
                TypedStmt::AnnAssign(ann_assign) => {
                    // Annotated attribute assignment: self.attr: Type = value
                    if let TypedExpr::Attribute(attr) = &ann_assign.target
                        && let TypedExpr::Name(_name) = &attr.value
                    {
                        // Check if this is a self attribute assignment
                        // Note: We can't easily check the symbol name here without access to the interner
                        // For now, we'll collect all attribute assignments
                        attributes.push((attr.attr, ann_assign.annotation.ty().clone()));
                    }
                }
                _ => {
                    // Recursively check nested statements
                    self.collect_attributes_from_nested_stmt(stmt, attributes);
                }
            }
        }
    }

    /// Collect methods from class body
    fn collect_methods_from_body(&self, body: &[TypedStmt<'a>], methods: &mut Vec<(Symbol, Type)>) {
        for stmt in body {
            match stmt {
                TypedStmt::FuncDef(func) => {
                    methods.push((func.name, func.ty.clone()));
                }
                _ => {
                    // Recursively check nested statements
                    self.collect_methods_from_nested_stmt(stmt, methods);
                }
            }
        }
    }

    /// Collect attributes from nested statements
    fn collect_attributes_from_nested_stmt(
        &self,
        stmt: &TypedStmt<'a>,
        attributes: &mut Vec<(Symbol, Type)>,
    ) {
        match stmt {
            TypedStmt::If(if_stmt) => {
                self.collect_attributes_from_body(if_stmt.body, attributes);
                self.collect_attributes_from_body(if_stmt.orelse, attributes);
            }
            TypedStmt::While(while_stmt) => {
                self.collect_attributes_from_body(while_stmt.body, attributes);
                self.collect_attributes_from_body(while_stmt.orelse, attributes);
            }
            TypedStmt::For(for_stmt) => {
                self.collect_attributes_from_body(for_stmt.body, attributes);
                self.collect_attributes_from_body(for_stmt.orelse, attributes);
            }
            TypedStmt::Try(try_stmt) => {
                self.collect_attributes_from_body(try_stmt.body, attributes);
                self.collect_attributes_from_body(try_stmt.orelse, attributes);
                self.collect_attributes_from_body(try_stmt.finalbody, attributes);
                for handler in try_stmt.handlers {
                    self.collect_attributes_from_body(handler.body, attributes);
                }
            }
            TypedStmt::With(with_stmt) => {
                self.collect_attributes_from_body(with_stmt.body, attributes);
            }
            TypedStmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_attributes_from_body(case.body, attributes);
                }
            }
            _ => {}
        }
    }

    /// Collect methods from nested statements
    fn collect_methods_from_nested_stmt(
        &self,
        stmt: &TypedStmt<'a>,
        methods: &mut Vec<(Symbol, Type)>,
    ) {
        match stmt {
            TypedStmt::If(if_stmt) => {
                self.collect_methods_from_body(if_stmt.body, methods);
                self.collect_methods_from_body(if_stmt.orelse, methods);
            }
            TypedStmt::While(while_stmt) => {
                self.collect_methods_from_body(while_stmt.body, methods);
                self.collect_methods_from_body(while_stmt.orelse, methods);
            }
            TypedStmt::For(for_stmt) => {
                self.collect_methods_from_body(for_stmt.body, methods);
                self.collect_methods_from_body(for_stmt.orelse, methods);
            }
            TypedStmt::Try(try_stmt) => {
                self.collect_methods_from_body(try_stmt.body, methods);
                self.collect_methods_from_body(try_stmt.orelse, methods);
                self.collect_methods_from_body(try_stmt.finalbody, methods);
                for handler in try_stmt.handlers {
                    self.collect_methods_from_body(handler.body, methods);
                }
            }
            TypedStmt::With(with_stmt) => {
                self.collect_methods_from_body(with_stmt.body, methods);
            }
            TypedStmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_methods_from_body(case.body, methods);
                }
            }
            _ => {}
        }
    }
}

/// Class analysis errors
#[derive(Debug, Clone)]
pub enum ClassAnalysisError {
    /// Circular inheritance detected
    CircularInheritance { class: Symbol, base: Symbol },
    /// MRO conflict (no valid linearization)
    MroConflict { class: Symbol, bases: Vec<Symbol> },
}

impl std::fmt::Display for ClassAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClassAnalysisError::CircularInheritance { class, base } => {
                write!(f, "Circular inheritance: {} inherits from {}", class, base)
            }
            ClassAnalysisError::MroConflict { class, bases } => {
                write!(f, "MRO conflict for class {} with bases {:?}", class, bases)
            }
        }
    }
}

impl std::error::Error for ClassAnalysisError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arena::symbol::Symbol;

    #[test]
    fn test_simple_inheritance() {
        let mut analyzer = ClassAnalyzer::new();

        // This is a simplified test - in practice, we'd need to create proper TypedClassDefStmt
        // For now, just test the MRO computation logic
        let class_a = Symbol::new(0);
        let class_b = Symbol::new(1);
        let class_c = Symbol::new(2);

        // A -> B -> C
        analyzer.inheritance_graph.insert(class_a, vec![class_b]);
        analyzer.inheritance_graph.insert(class_b, vec![class_c]);
        analyzer.inheritance_graph.insert(class_c, vec![]);

        let mro_a = analyzer.compute_mro(class_a).unwrap();
        assert_eq!(mro_a, vec![class_a, class_b, class_c]);
    }

    #[test]
    fn test_diamond_inheritance() {
        let mut analyzer = ClassAnalyzer::new();

        let class_a = Symbol::new(0);
        let class_b = Symbol::new(1);
        let class_c = Symbol::new(2);
        let class_d = Symbol::new(3);

        // Diamond inheritance: D -> B, C; B -> A; C -> A
        analyzer
            .inheritance_graph
            .insert(class_d, vec![class_b, class_c]);
        analyzer.inheritance_graph.insert(class_b, vec![class_a]);
        analyzer.inheritance_graph.insert(class_c, vec![class_a]);
        analyzer.inheritance_graph.insert(class_a, vec![]);

        let mro_d = analyzer.compute_mro(class_d).unwrap();
        // Should be D, B, C, A (C3 linearization)
        assert_eq!(mro_d, vec![class_d, class_b, class_c, class_a]);
    }
}
