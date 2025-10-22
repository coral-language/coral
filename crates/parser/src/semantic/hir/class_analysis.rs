//! Class hierarchy analysis and MRO computation

use super::typed_expr::TypedExpr;
use super::typed_stmt::TypedStmt;
use crate::arena::interner::Interner;
use crate::arena::symbol::Symbol;
use crate::semantic::types::{AttributeKind, Type};
use std::collections::{HashMap, HashSet};
use text_size::TextRange;

/// Type alias for attribute collection return type
type AttributeCollectionResult = (Vec<(Symbol, Type)>, Vec<(Symbol, Type)>);

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
    /// Property descriptors for each class (class -> attr -> PropertyDescriptor)
    property_descriptors: HashMap<Symbol, HashMap<Symbol, PropertyDescriptor>>,
    /// Class-level attributes (vs instance attributes)
    class_attributes: HashMap<Symbol, HashMap<Symbol, Type>>,
    /// Cache for resolved attribute types: (class, attr) -> resolved type
    attribute_cache: HashMap<(Symbol, Symbol), Type>,
    /// Reference to interner for decorator name resolution
    interner: Option<&'a Interner>,
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

/// Method decorator types
#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
enum MethodDecorator {
    None,
    StaticMethod,
    ClassMethod,
    InstanceMethod,
}

/// Property descriptor information
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDescriptor {
    /// The getter method type
    pub getter_type: Type,
    /// The setter method type (if exists)
    pub setter_type: Option<Type>,
    /// The attribute name
    pub attr_name: Symbol,
    /// The class this property belongs to
    pub class_name: Symbol,
}

impl<'a> ClassAnalyzer<'a> {
    /// Create a new class analyzer
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the interner for this analyzer (needed for decorator name resolution)
    pub fn set_interner(&mut self, interner: &'a Interner) {
        self.interner = Some(interner);
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

    /// Resolve attribute type for a given class and attribute name
    pub fn resolve_attribute_type(&self, class_name: Symbol, attr_name: Symbol) -> Option<Type> {
        self.resolve_attribute_type_uncached(class_name, attr_name)
    }

    /// Check cache, then walk MRO to find attribute
    pub fn resolve_attribute_with_cache(
        &mut self,
        class_name: Symbol,
        attr_name: Symbol,
    ) -> Option<Type> {
        let key = (class_name, attr_name);

        // Check cache first
        if let Some(cached) = self.attribute_cache.get(&key) {
            return Some(cached.clone());
        }

        // Compute and cache
        if let Some(ty) = self.resolve_attribute_type_uncached(class_name, attr_name) {
            self.attribute_cache.insert(key, ty.clone());
            Some(ty)
        } else {
            None
        }
    }

    /// Distinguish instance vs class attributes
    pub fn is_class_attribute(&self, class_name: Symbol, attr_name: Symbol) -> bool {
        self.class_attributes
            .get(&class_name)
            .map(|attrs| attrs.contains_key(&attr_name))
            .unwrap_or(false)
    }

    /// Get property descriptor if attribute is a property
    pub fn get_property_descriptor(
        &self,
        class_name: Symbol,
        attr_name: Symbol,
    ) -> Option<&PropertyDescriptor> {
        self.property_descriptors
            .get(&class_name)
            .and_then(|props| props.get(&attr_name))
    }

    /// Clear cache when class definitions change
    pub fn invalidate_cache(&mut self) {
        self.attribute_cache.clear();
    }

    /// Resolve attribute type without caching (internal implementation)
    fn resolve_attribute_type_uncached(
        &self,
        class_name: Symbol,
        attr_name: Symbol,
    ) -> Option<Type> {
        // Check property descriptors first
        if let Some(prop) = self.get_property_descriptor(class_name, attr_name) {
            // Return as AttributeDescriptor type
            return Some(Type::AttributeDescriptor {
                kind: AttributeKind::Property,
                getter_type: Box::new(prop.getter_type.clone()),
                setter_type: prop.setter_type.as_ref().map(|t| Box::new(t.clone())),
            });
        }

        // Check class attributes
        if let Some(class_attrs) = self.class_attributes.get(&class_name)
            && let Some(ty) = class_attrs.get(&attr_name)
        {
            return Some(ty.clone());
        }

        // Check instance attribute tables
        if let Some(attrs) = self.attribute_tables.get(&class_name)
            && let Some((_, ty)) = attrs.iter().find(|(name, _)| *name == attr_name)
        {
            return Some(ty.clone());
        }

        // Walk MRO for inherited attributes
        if let Some(mro) = self.get_mro(class_name) {
            for &base_class in mro.iter().skip(1) {
                // Skip self
                if let Some(ty) = self.resolve_attribute_type_uncached(base_class, attr_name) {
                    return Some(ty);
                }
            }
        }

        None
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
        let mut instance_attributes = Vec::new();
        let mut class_attributes = HashMap::new();
        let mut properties = HashMap::new();

        // Walk through the class body to find attribute assignments and properties
        self.collect_attributes_from_body_enhanced(
            class.body,
            &mut instance_attributes,
            &mut class_attributes,
            &mut properties,
            class.name,
        );

        // Add attributes from base classes (following MRO)
        if let Some(mro) = self.mro_cache.get(&class.name) {
            for &base_class in mro.iter().skip(1) {
                // Skip the class itself
                if let Some(base_attributes) = self.attribute_tables.get(&base_class) {
                    for (name, ty) in base_attributes.iter() {
                        // Only add if not already defined
                        if !instance_attributes.iter().any(|(n, _)| *n == *name) {
                            instance_attributes.push((*name, ty.clone()));
                        }
                    }
                }

                // Add base class attributes
                if let Some(base_class_attrs) = self.class_attributes.get(&base_class) {
                    for (name, ty) in base_class_attrs.iter() {
                        if !class_attributes.contains_key(name) {
                            class_attributes.insert(*name, ty.clone());
                        }
                    }
                }

                // Add base class properties
                if let Some(base_props) = self.property_descriptors.get(&base_class) {
                    for (name, prop) in base_props.iter() {
                        if !properties.contains_key(name) {
                            properties.insert(*name, prop.clone());
                        }
                    }
                }
            }
        }

        self.attribute_tables
            .insert(class.name, instance_attributes);
        self.class_attributes.insert(class.name, class_attributes);
        self.property_descriptors.insert(class.name, properties);
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

    /// Enhanced attribute collection that distinguishes properties, class attrs, and instance attrs
    fn collect_attributes_from_body_enhanced(
        &self,
        body: &[TypedStmt<'a>],
        instance_attributes: &mut Vec<(Symbol, Type)>,
        class_attributes: &mut HashMap<Symbol, Type>,
        properties: &mut HashMap<Symbol, PropertyDescriptor>,
        class_name: Symbol,
    ) {
        for stmt in body {
            match stmt {
                TypedStmt::FuncDef(func) => {
                    // Validate decorator combinations
                    if !self.validate_decorator_combination(func.decorators) {
                        // In a full implementation, this would emit a warning or error
                        // For now, we silently continue to avoid breaking type inference
                    }

                    // Check for property decorators
                    if let Some(prop_desc) =
                        self.extract_property_descriptor(func, class_name, body)
                    {
                        properties.insert(prop_desc.attr_name, prop_desc);
                    } else {
                        // Check for classmethod/staticmethod decorators
                        let decorator_kind = self.detect_method_decorator(func.decorators);
                        match decorator_kind {
                            MethodDecorator::StaticMethod => {
                                // Static methods are class attributes
                                class_attributes.insert(func.name, func.ty.clone());
                            }
                            MethodDecorator::ClassMethod => {
                                // Class methods are class attributes
                                class_attributes.insert(func.name, func.ty.clone());
                            }
                            MethodDecorator::InstanceMethod | MethodDecorator::None => {
                                // Regular instance methods are handled in build_method_table
                            }
                        }
                    }
                }
                TypedStmt::Assign(assign) => {
                    // Check if this is a class-level assignment
                    if self.is_class_level_assignment(assign) {
                        // Class attribute assignment
                        for target in assign.targets {
                            if let TypedExpr::Name(name) = target {
                                class_attributes.insert(name.symbol, assign.value.ty().clone());
                            }
                        }
                    } else {
                        // Instance attribute assignment
                        self.collect_instance_attributes_from_assign(assign, instance_attributes);
                    }
                }
                TypedStmt::AnnAssign(ann_assign) => {
                    // Check if this is a class-level annotation
                    if self.is_class_level_annotation(ann_assign) {
                        // Class attribute annotation
                        if let TypedExpr::Name(name) = &ann_assign.target {
                            class_attributes
                                .insert(name.symbol, ann_assign.annotation.ty().clone());
                        }
                    } else {
                        // Instance attribute annotation
                        self.collect_instance_attributes_from_ann_assign(
                            ann_assign,
                            instance_attributes,
                        );
                    }
                }
                _ => {
                    // Recursively check nested statements
                    self.collect_attributes_from_nested_stmt_enhanced(
                        stmt,
                        instance_attributes,
                        class_attributes,
                        properties,
                        class_name,
                    );
                }
            }
        }
    }

    /// Extract property descriptor from a function definition
    fn extract_property_descriptor(
        &self,
        func: &super::typed_stmt::TypedFuncDefStmt<'a>,
        class_name: Symbol,
        body: &[TypedStmt<'a>],
    ) -> Option<PropertyDescriptor> {
        if self.has_property_decorator(func.decorators) {
            // Find the property name (same as function name)
            let attr_name = func.name;
            let getter_type = func.ty.clone();

            // Look for a corresponding setter method (@attr_name.setter)
            let setter_type = self.find_property_setter(attr_name, body);

            Some(PropertyDescriptor {
                getter_type,
                setter_type,
                attr_name,
                class_name,
            })
        } else {
            None
        }
    }

    /// Find property setter method for a given property name
    fn find_property_setter(&self, property_name: Symbol, body: &[TypedStmt<'a>]) -> Option<Type> {
        // Look for a method decorated with @property_name.setter
        for stmt in body {
            if let TypedStmt::FuncDef(func) = stmt {
                // Check if any decorator matches the setter pattern
                for decorator in func.decorators {
                    if let TypedExpr::Attribute(attr) = decorator {
                        // Check if it matches pattern: property_name.setter
                        if attr.attr == property_name
                            && let TypedExpr::Name(base) = &attr.value
                            && base.symbol == property_name
                        {
                            // This is a setter for the property
                            return Some(func.ty.clone());
                        }
                    }
                }
            }
        }
        None
    }

    /// Check if function has @property decorator
    fn has_property_decorator(&self, decorators: &[TypedExpr<'a>]) -> bool {
        match self.interner {
            Some(interner) => {
                for decorator in decorators {
                    if let TypedExpr::Name(name_expr) = decorator {
                        if let Some("property") = interner.resolve(name_expr.symbol) {
                            return true;
                        }
                    } else if let TypedExpr::Attribute(attr) = decorator
                        && let TypedExpr::Name(base) = attr.value
                        && let Some("property") = interner.resolve(base.symbol)
                    {
                        // Handle @foo.property style decorators
                        return true;
                    }
                }
                false
            }
            None => {
                // Interner not set - can't resolve decorator names
                // In production, this should be logged as a warning
                false
            }
        }
    }

    /// Detect method decorators (@staticmethod, @classmethod)
    fn detect_method_decorator(&self, decorators: &[TypedExpr<'a>]) -> MethodDecorator {
        match self.interner {
            Some(interner) => {
                for decorator in decorators {
                    if let TypedExpr::Name(name_expr) = decorator {
                        match interner.resolve(name_expr.symbol) {
                            Some("staticmethod") => return MethodDecorator::StaticMethod,
                            Some("classmethod") => return MethodDecorator::ClassMethod,
                            _ => {}
                        }
                    }
                }
                MethodDecorator::None
            }
            None => {
                // Interner not set - can't resolve decorator names
                MethodDecorator::None
            }
        }
    }

    /// Validate decorator combinations for correctness
    /// Returns true if decorators are valid, false otherwise
    fn validate_decorator_combination(&self, decorators: &[TypedExpr<'a>]) -> bool {
        if decorators.is_empty() {
            return true;
        }

        let mut has_property = false;
        let mut has_staticmethod = false;
        let mut has_classmethod = false;

        if let Some(interner) = self.interner {
            for decorator in decorators {
                if let TypedExpr::Name(name_expr) = decorator
                    && let Some(name) = interner.resolve(name_expr.symbol)
                {
                    match name {
                        "property" => has_property = true,
                        "staticmethod" => has_staticmethod = true,
                        "classmethod" => has_classmethod = true,
                        _ => {}
                    }
                }
            }
        }

        // Check invalid combinations
        // @property cannot be combined with @staticmethod or @classmethod
        !(has_property && (has_staticmethod || has_classmethod)) &&
        // @staticmethod and @classmethod are mutually exclusive
        !(has_staticmethod && has_classmethod)
    }

    /// Check if assignment is at class level (not in a method)
    fn is_class_level_assignment(&self, assign: &super::typed_stmt::TypedAssignStmt<'a>) -> bool {
        // Check if any target is a self attribute assignment
        for target in assign.targets {
            if let TypedExpr::Attribute(_attr) = target {
                // If target is an attribute access, it's an instance assignment
                // (e.g., self.value = 10 or obj.field = value)
                return false;
            }
        }
        // Otherwise, it's a class-level assignment (e.g., count = 0)
        true
    }

    /// Check if annotation is at class level
    fn is_class_level_annotation(
        &self,
        ann_assign: &super::typed_stmt::TypedAnnAssignStmt<'a>,
    ) -> bool {
        // If target is an attribute access, it's an instance annotation
        if let TypedExpr::Attribute(_attr) = &ann_assign.target {
            return false;
        }
        // Otherwise, it's a class-level annotation (e.g., count: int = 0)
        true
    }

    /// Collect instance attributes from assignment
    fn collect_instance_attributes_from_assign(
        &self,
        assign: &super::typed_stmt::TypedAssignStmt<'a>,
        attributes: &mut Vec<(Symbol, Type)>,
    ) {
        for target in assign.targets {
            if let TypedExpr::Attribute(attr) = target {
                // Check if this is a self attribute assignment or any attribute
                if let TypedExpr::Name(_name) = &attr.value {
                    // Note: In a full implementation, we'd verify this is 'self'
                    // For now, we collect all attribute assignments as instance attributes
                    attributes.push((attr.attr, assign.value.ty().clone()));
                }
            }
        }
    }

    /// Collect instance attributes from annotated assignment
    fn collect_instance_attributes_from_ann_assign(
        &self,
        ann_assign: &super::typed_stmt::TypedAnnAssignStmt<'a>,
        attributes: &mut Vec<(Symbol, Type)>,
    ) {
        if let TypedExpr::Attribute(attr) = &ann_assign.target {
            // Check if this is a self attribute assignment or any attribute
            if let TypedExpr::Name(_name) = &attr.value {
                attributes.push((attr.attr, ann_assign.annotation.ty().clone()));
            }
        }
    }

    /// Enhanced nested statement collection
    fn collect_attributes_from_nested_stmt_enhanced(
        &self,
        stmt: &TypedStmt<'a>,
        instance_attributes: &mut Vec<(Symbol, Type)>,
        class_attributes: &mut HashMap<Symbol, Type>,
        properties: &mut HashMap<Symbol, PropertyDescriptor>,
        class_name: Symbol,
    ) {
        match stmt {
            TypedStmt::If(if_stmt) => {
                self.collect_attributes_from_body_enhanced(
                    if_stmt.body,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
                self.collect_attributes_from_body_enhanced(
                    if_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
            }
            TypedStmt::While(while_stmt) => {
                self.collect_attributes_from_body_enhanced(
                    while_stmt.body,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
                self.collect_attributes_from_body_enhanced(
                    while_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
            }
            TypedStmt::For(for_stmt) => {
                self.collect_attributes_from_body_enhanced(
                    for_stmt.body,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
                self.collect_attributes_from_body_enhanced(
                    for_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
            }
            TypedStmt::Try(try_stmt) => {
                self.collect_attributes_from_body_enhanced(
                    try_stmt.body,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
                self.collect_attributes_from_body_enhanced(
                    try_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
                self.collect_attributes_from_body_enhanced(
                    try_stmt.finalbody,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
                for handler in try_stmt.handlers {
                    self.collect_attributes_from_body_enhanced(
                        handler.body,
                        instance_attributes,
                        class_attributes,
                        properties,
                        class_name,
                    );
                }
            }
            TypedStmt::With(with_stmt) => {
                self.collect_attributes_from_body_enhanced(
                    with_stmt.body,
                    instance_attributes,
                    class_attributes,
                    properties,
                    class_name,
                );
            }
            TypedStmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_attributes_from_body_enhanced(
                        case.body,
                        instance_attributes,
                        class_attributes,
                        properties,
                        class_name,
                    );
                }
            }
            _ => {}
        }
    }

    /// Collect attributes from a class body (deprecated in favor of collect_attributes_from_body_enhanced)
    #[allow(dead_code)]
    fn collect_attributes_from_body(&self, body: &[TypedStmt<'a>]) -> AttributeCollectionResult {
        let mut instance_attributes = Vec::new();
        let mut class_attributes = Vec::new();

        for stmt in body {
            match stmt {
                TypedStmt::Assign(assign) => {
                    if self.is_class_level_assignment(assign) {
                        // Class attribute
                        for target in assign.targets {
                            if let TypedExpr::Name(name) = target {
                                class_attributes.push((name.symbol, assign.value.ty().clone()));
                            }
                        }
                    }
                }
                TypedStmt::AnnAssign(ann_assign) => {
                    if self.is_class_level_annotation(ann_assign) {
                        // Class attribute annotation
                        if let TypedExpr::Name(name) = &ann_assign.target {
                            class_attributes
                                .push((name.symbol, ann_assign.annotation.ty().clone()));
                        }
                    } else {
                        // Instance attribute annotation
                        self.collect_instance_attributes_from_ann_assign(
                            ann_assign,
                            &mut instance_attributes,
                        );
                    }
                }
                _ => {}
            }
        }

        (instance_attributes, class_attributes)
    }

    /// Collect attributes from nested statements
    #[allow(dead_code, clippy::only_used_in_recursion)]
    fn collect_attributes_from_nested_stmt(
        &self,
        stmt: &TypedStmt<'a>,
    ) -> AttributeCollectionResult {
        let mut instance_attributes = Vec::new();
        let mut class_attributes = Vec::new();

        match stmt {
            TypedStmt::If(if_stmt) => {
                for s in if_stmt.body {
                    let (inst_attrs, class_attrs) = self.collect_attributes_from_nested_stmt(s);
                    instance_attributes.extend(inst_attrs);
                    class_attributes.extend(class_attrs);
                }
                for s in if_stmt.orelse {
                    let (inst_attrs, class_attrs) = self.collect_attributes_from_nested_stmt(s);
                    instance_attributes.extend(inst_attrs);
                    class_attributes.extend(class_attrs);
                }
            }
            TypedStmt::While(while_stmt) => {
                for s in while_stmt.body {
                    let (inst_attrs, class_attrs) = self.collect_attributes_from_nested_stmt(s);
                    instance_attributes.extend(inst_attrs);
                    class_attributes.extend(class_attrs);
                }
            }
            TypedStmt::For(for_stmt) => {
                for s in for_stmt.body {
                    let (inst_attrs, class_attrs) = self.collect_attributes_from_nested_stmt(s);
                    instance_attributes.extend(inst_attrs);
                    class_attributes.extend(class_attrs);
                }
            }
            TypedStmt::Try(try_stmt) => {
                for s in try_stmt.body {
                    let (inst_attrs, class_attrs) = self.collect_attributes_from_nested_stmt(s);
                    instance_attributes.extend(inst_attrs);
                    class_attributes.extend(class_attrs);
                }
            }
            _ => {}
        }

        (instance_attributes, class_attributes)
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
