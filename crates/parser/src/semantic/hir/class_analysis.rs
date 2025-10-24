//! Class hierarchy analysis and MRO computation

use super::typed_expr::TypedExpr;
use super::typed_stmt::TypedStmt;
use crate::arena::interner::Interner;
use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use std::collections::{HashMap, HashSet};
use text_size::TextRange;

/// Comprehensive metadata for a single class
/// This is the single source of truth for class information after analysis
#[derive(Debug, Clone)]
pub struct ClassMetadata {
    /// Class name
    pub name: String,
    /// Whether this is a protocol definition
    pub is_protocol: bool,
    /// Protocols this class implements
    pub implements: Vec<String>,
    /// Method resolution order (MRO) - list of base class names in order
    pub mro: Vec<String>,
    /// Instance attributes (name -> type)
    pub instance_attributes: HashMap<String, Type>,
    /// Class-level attributes (name -> type)
    pub class_attributes: HashMap<String, Type>,
    /// Methods (name -> type)
    pub methods: HashMap<String, Type>,
    /// Property descriptors (name -> PropertyDescriptor)
    pub properties: HashMap<String, PropertyDescriptor>,
    /// Constructor signature (if present)
    pub constructor: Option<Type>,
}

/// Class hierarchy analyzer that computes MRO and attribute tables
pub struct ClassAnalyzer<'a> {
    /// All classes in the module
    classes: Vec<&'a TypedClassDefStmt<'a>>,
    /// Inheritance graph (class -> direct bases)
    inheritance_graph: HashMap<Symbol, Vec<Symbol>>,
    /// Computed metadata for each class (single source of truth)
    class_metadata: HashMap<Symbol, ClassMetadata>,
    /// Reference to interner for decorator name resolution and string conversion
    interner: &'a Interner,
}

impl<'a> Default for ClassAnalyzer<'a> {
    fn default() -> Self {
        // Create a static empty interner for default initialization
        // This will be replaced by set_interner before actual use
        static EMPTY_INTERNER: once_cell::sync::Lazy<Interner> =
            once_cell::sync::Lazy::new(Interner::new);
        Self {
            classes: Vec::new(),
            inheritance_graph: HashMap::new(),
            class_metadata: HashMap::new(),
            interner: &EMPTY_INTERNER,
        }
    }
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
    pub is_protocol: bool,
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
    Operator,
}

/// Property descriptor information
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDescriptor {
    /// The getter method type
    pub getter_type: Type,
    /// The setter method type (if exists)
    pub setter_type: Option<Type>,
    /// The deleter method type (if exists)
    pub deleter_type: Option<Type>,
    /// The attribute name
    pub attr_name: Symbol,
    /// The class this property belongs to
    pub class_name: Symbol,
}

impl<'a> ClassAnalyzer<'a> {
    /// Create a new class analyzer with an interner reference
    pub fn new(interner: &'a Interner) -> Self {
        Self {
            classes: Vec::new(),
            inheritance_graph: HashMap::new(),
            class_metadata: HashMap::new(),
            interner,
        }
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

    /// Analyze all classes and compute MRO and comprehensive metadata
    pub fn analyze(&mut self) -> Result<(), ClassAnalysisError> {
        // Check for circular inheritance
        self.check_circular_inheritance()?;

        // Compute MRO for all classes
        let class_names: Vec<Symbol> = self.classes.iter().map(|c| c.name).collect();
        let mut mro_cache: HashMap<Symbol, Vec<Symbol>> = HashMap::new();
        for class_name in class_names {
            let mro = self.compute_mro(class_name)?;
            mro_cache.insert(class_name, mro);
        }

        // Build comprehensive metadata for each class
        let class_refs: Vec<&TypedClassDefStmt<'a>> = self.classes.to_vec();
        for class in class_refs {
            let metadata = self.build_class_metadata(class, &mro_cache);
            self.class_metadata.insert(class.name, metadata);
        }

        Ok(())
    }

    /// Get metadata for a class
    pub fn get_metadata(&self, class_name: Symbol) -> Option<&ClassMetadata> {
        self.class_metadata.get(&class_name)
    }

    /// Export all class metadata (consumes the analyzer)
    pub fn export_metadata(self) -> HashMap<String, ClassMetadata> {
        self.class_metadata
            .into_iter()
            .map(|(sym, meta)| {
                // Metadata already has String keys, just need to convert Symbol to String
                let class_name = self
                    .interner
                    .resolve(sym)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("UnknownClass_{}", sym.as_u32()));
                (class_name, meta)
            })
            .collect()
    }

    /// Build comprehensive metadata for a single class
    fn build_class_metadata(
        &self,
        class: &TypedClassDefStmt<'a>,
        mro_cache: &HashMap<Symbol, Vec<Symbol>>,
    ) -> ClassMetadata {
        let class_name_str = self
            .interner
            .resolve(class.name)
            .map(|s| s.to_string())
            .unwrap_or_else(|| format!("UnknownClass_{}", class.name.as_u32()));

        // Convert MRO to string names
        let mro = mro_cache
            .get(&class.name)
            .map(|mro_syms| {
                mro_syms
                    .iter()
                    .filter_map(|sym| self.interner.resolve(*sym).map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_else(|| vec![class_name_str.clone()]);

        let mut instance_attributes = HashMap::new();
        let mut class_attributes = HashMap::new();
        let mut methods = HashMap::new();
        let mut properties = HashMap::new();
        let mut constructor = None;

        // Collect from class body
        self.collect_class_members(
            class.body,
            &mut instance_attributes,
            &mut class_attributes,
            &mut methods,
            &mut properties,
            &mut constructor,
            class.name,
        );

        // Inherit from base classes following MRO
        if let Some(mro_syms) = mro_cache.get(&class.name) {
            for &base_sym in mro_syms.iter().skip(1) {
                if let Some(base_meta) = self.class_metadata.get(&base_sym) {
                    // Inherit instance attributes
                    for (name, ty) in &base_meta.instance_attributes {
                        instance_attributes
                            .entry(name.clone())
                            .or_insert_with(|| ty.clone());
                    }
                    // Inherit class attributes
                    for (name, ty) in &base_meta.class_attributes {
                        class_attributes
                            .entry(name.clone())
                            .or_insert_with(|| ty.clone());
                    }
                    // Inherit methods
                    for (name, ty) in &base_meta.methods {
                        methods.entry(name.clone()).or_insert_with(|| ty.clone());
                    }
                    // Inherit properties
                    for (name, prop) in &base_meta.properties {
                        properties
                            .entry(name.clone())
                            .or_insert_with(|| prop.clone());
                    }
                }
            }
        }

        // Extract protocol implementations from bases
        let implements = class
            .bases
            .iter()
            .filter_map(|base| {
                if let TypedExpr::Name(name_expr) = base {
                    self.interner
                        .resolve(name_expr.symbol)
                        .map(|s| s.to_string())
                } else {
                    None
                }
            })
            .collect();

        ClassMetadata {
            name: class_name_str,
            is_protocol: class.is_protocol,
            implements,
            mro,
            instance_attributes,
            class_attributes,
            methods,
            properties,
            constructor,
        }
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

    /// Collect all members from a class body
    /// This is the core method that addresses all attribute collection issues
    #[allow(clippy::too_many_arguments)]
    fn collect_class_members(
        &self,
        body: &[TypedStmt<'a>],
        instance_attributes: &mut HashMap<String, Type>,
        class_attributes: &mut HashMap<String, Type>,
        methods: &mut HashMap<String, Type>,
        properties: &mut HashMap<String, PropertyDescriptor>,
        constructor: &mut Option<Type>,
        class_name: Symbol,
    ) {
        for stmt in body {
            match stmt {
                TypedStmt::FuncDef(func) => {
                    let func_name = self
                        .interner
                        .resolve(func.name)
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| format!("unknown_func_{}", func.name.as_u32()));

                    // Validate decorator combination and report errors if invalid
                    if let Err(err_msg) =
                        self.validate_decorator_combination_with_errors(func.decorators)
                    {
                        // Decorator validation errors will be reported by decorator_resolution pass
                        // But we still need to log them for debugging
                        eprintln!(
                            "Decorator validation warning for {}: {}",
                            func_name, err_msg
                        );
                    }

                    // Check for constructor method (Coral uses "constructor" not "__init__")
                    if func_name == "constructor" {
                        *constructor = Some(func.ty.clone());
                        // Constructor is also added as a regular method
                        methods.insert(func_name, func.ty.clone());
                        continue;
                    }

                    // Check for property decorators
                    if self.has_property_decorator(func.decorators) {
                        if let Some(prop_desc) =
                            self.extract_property_descriptor(func, class_name, body)
                        {
                            let prop_name = self
                                .interner
                                .resolve(prop_desc.attr_name)
                                .map(|s| s.to_string())
                                .unwrap_or_default();
                            properties.insert(prop_name, prop_desc);
                        }
                        continue;
                    }

                    // Detect method decorator type
                    let decorator_kind = self.detect_method_decorator(func.decorators);
                    match decorator_kind {
                        MethodDecorator::StaticMethod => {
                            // Static methods are class attributes
                            class_attributes.insert(func_name, func.ty.clone());
                        }
                        MethodDecorator::ClassMethod => {
                            // Class methods are class attributes
                            class_attributes.insert(func_name, func.ty.clone());
                        }
                        MethodDecorator::Operator => {
                            // Validate operator signature
                            if let Err(_err) = self.validate_operator_signature(func) {
                                // Operator validation errors reported by decorator_resolution pass
                            }
                            // Operator methods are instance methods with special behavior
                            methods.insert(func_name, func.ty.clone());
                        }
                        MethodDecorator::InstanceMethod | MethodDecorator::None => {
                            // Regular instance methods
                            methods.insert(func_name, func.ty.clone());
                        }
                    }
                }
                TypedStmt::Assign(assign) => {
                    if self.is_class_level_assignment(assign) {
                        // Class attribute assignment
                        for target in assign.targets {
                            if let TypedExpr::Name(name) = target
                                && let Some(name_str) = self.interner.resolve(name.symbol)
                            {
                                class_attributes
                                    .insert(name_str.to_string(), assign.value.ty().clone());
                            }
                        }
                    } else {
                        // Instance attribute assignment - validate self parameter
                        for target in assign.targets {
                            if let TypedExpr::Attribute(attr) = target
                                && self.is_self_attribute(attr.value)
                                && let Some(attr_name) = self.interner.resolve(attr.attr)
                            {
                                instance_attributes
                                    .insert(attr_name.to_string(), assign.value.ty().clone());
                            }
                        }
                    }
                }
                TypedStmt::AnnAssign(ann_assign) => {
                    if self.is_class_level_annotation(ann_assign) {
                        // Class attribute annotation - use the type from typed annotation
                        if let TypedExpr::Name(name) = &ann_assign.target {
                            let name_str = self.interner.resolve(name.symbol);

                            if let Some(name_str) = name_str {
                                // Parse the annotation to get the actual type (int, str, etc.)
                                let ty = self.extract_type_from_annotation(&ann_assign.annotation);
                                class_attributes.insert(name_str.to_string(), ty);
                            }
                        }
                    } else {
                        // Instance attribute annotation - validate self
                        if let TypedExpr::Attribute(attr) = &ann_assign.target
                            && self.is_self_attribute(attr.value)
                            && let Some(attr_name) = self.interner.resolve(attr.attr)
                        {
                            let ty = self.extract_type_from_annotation(&ann_assign.annotation);
                            instance_attributes.insert(attr_name.to_string(), ty);
                        }
                    }
                }
                _ => {
                    // Recursively check nested statements
                    self.collect_class_members_from_nested_stmt(
                        stmt,
                        instance_attributes,
                        class_attributes,
                        methods,
                        properties,
                        constructor,
                        class_name,
                    );
                }
            }
        }
    }

    /// Check if an expression is a reference to `self`
    fn is_self_attribute(&self, expr: &TypedExpr<'a>) -> bool {
        if let TypedExpr::Name(name) = expr
            && let Some("self") = self.interner.resolve(name.symbol)
        {
            return true;
        }
        false
    }

    /// Extract type from an annotation expression
    fn extract_type_from_annotation(&self, annotation: &TypedExpr<'a>) -> Type {
        match annotation {
            TypedExpr::Name(name) => {
                // Simple type names
                if let Some(type_name) = self.interner.resolve(name.symbol) {
                    match type_name {
                        "int" => Type::Int,
                        "str" => Type::Str,
                        "float" => Type::Float,
                        "bool" => Type::Bool,
                        "bytes" => Type::Bytes,
                        "complex" => Type::Complex,
                        "None" => Type::None,
                        "Any" => Type::Any,
                        _ => Type::Instance(type_name.to_string()), // Custom class
                    }
                } else {
                    Type::Unknown
                }
            }
            // Add support for generic types if needed in the future
            _ => Type::Unknown,
        }
    }

    /// Compute MRO using C3 linearization algorithm (no longer needs caching internally)
    fn compute_mro(&self, class_name: Symbol) -> Result<Vec<Symbol>, ClassAnalysisError> {
        let bases = self
            .inheritance_graph
            .get(&class_name)
            .cloned()
            .unwrap_or_default();

        if bases.is_empty() {
            // No bases, MRO is just the class itself
            return Ok(vec![class_name]);
        }

        // C3 linearization algorithm
        let mut mro = vec![class_name];

        // Get MROs for all bases (recursive)
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

        Ok(mro)
    }

    /// Collect class members from nested statements
    #[allow(clippy::too_many_arguments)]
    fn collect_class_members_from_nested_stmt(
        &self,
        stmt: &TypedStmt<'a>,
        instance_attributes: &mut HashMap<String, Type>,
        class_attributes: &mut HashMap<String, Type>,
        methods: &mut HashMap<String, Type>,
        properties: &mut HashMap<String, PropertyDescriptor>,
        constructor: &mut Option<Type>,
        class_name: Symbol,
    ) {
        match stmt {
            TypedStmt::If(if_stmt) => {
                self.collect_class_members(
                    if_stmt.body,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
                self.collect_class_members(
                    if_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
            }
            TypedStmt::While(while_stmt) => {
                self.collect_class_members(
                    while_stmt.body,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
                self.collect_class_members(
                    while_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
            }
            TypedStmt::For(for_stmt) => {
                self.collect_class_members(
                    for_stmt.body,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
                self.collect_class_members(
                    for_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
            }
            TypedStmt::Try(try_stmt) => {
                self.collect_class_members(
                    try_stmt.body,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
                self.collect_class_members(
                    try_stmt.orelse,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
                self.collect_class_members(
                    try_stmt.finalbody,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
                for handler in try_stmt.handlers {
                    self.collect_class_members(
                        handler.body,
                        instance_attributes,
                        class_attributes,
                        methods,
                        properties,
                        constructor,
                        class_name,
                    );
                }
            }
            TypedStmt::With(with_stmt) => {
                self.collect_class_members(
                    with_stmt.body,
                    instance_attributes,
                    class_attributes,
                    methods,
                    properties,
                    constructor,
                    class_name,
                );
            }
            TypedStmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_class_members(
                        case.body,
                        instance_attributes,
                        class_attributes,
                        methods,
                        properties,
                        constructor,
                        class_name,
                    );
                }
            }
            _ => {}
        }
    }

    /// Validate operator signature
    fn validate_operator_signature(
        &self,
        func: &super::typed_stmt::TypedFuncDefStmt<'a>,
    ) -> Result<(), String> {
        let func_name = self.interner.resolve(func.name).unwrap_or("unknown");

        // Operator methods must have at least self parameter
        let param_count = func.args.args.len();
        if param_count == 0 {
            return Err(format!(
                "Operator method {} must have at least self parameter",
                func_name
            ));
        }

        // Binary operators need exactly 2 parameters (self + other)
        let binary_ops = [
            "add", "sub", "mul", "div", "mod", "pow", "floordiv", "and", "or", "xor", "lshift",
            "rshift", "eq", "ne", "lt", "le", "gt", "ge",
        ];

        if binary_ops.contains(&func_name) && param_count != 2 {
            return Err(format!(
                "Binary operator {} must have exactly 2 parameters (self, other), found {}",
                func_name, param_count
            ));
        }

        // Unary operators need exactly 1 parameter (self)
        let unary_ops = ["neg", "pos", "invert", "abs"];
        if unary_ops.contains(&func_name) && param_count != 1 {
            return Err(format!(
                "Unary operator {} must have exactly 1 parameter (self), found {}",
                func_name, param_count
            ));
        }

        Ok(())
    }

    /// Validate decorator combination with detailed error messages
    fn validate_decorator_combination_with_errors(
        &self,
        decorators: &[TypedExpr<'a>],
    ) -> Result<(), String> {
        if decorators.is_empty() {
            return Ok(());
        }

        let mut has_property = false;
        let mut has_staticmethod = false;
        let mut has_classmethod = false;

        for decorator in decorators {
            if let TypedExpr::Name(name_expr) = decorator
                && let Some(name) = self.interner.resolve(name_expr.symbol)
            {
                match name {
                    "property" => has_property = true,
                    "staticmethod" => has_staticmethod = true,
                    "classmethod" => has_classmethod = true,
                    _ => {}
                }
            }
        }

        // Check invalid combinations
        if has_property && has_staticmethod {
            Err("@property cannot be combined with @staticmethod".to_string())
        } else if has_property && has_classmethod {
            Err("@property cannot be combined with @classmethod".to_string())
        } else if has_staticmethod && has_classmethod {
            Err("@staticmethod and @classmethod are mutually exclusive".to_string())
        } else {
            Ok(())
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

            // Look for a corresponding deleter method (@attr_name.deleter)
            let deleter_type = self.find_property_deleter(attr_name, body);

            Some(PropertyDescriptor {
                getter_type,
                setter_type,
                deleter_type,
                attr_name,
                class_name,
            })
        } else {
            None
        }
    }

    /// Find property setter method for a given property name
    fn find_property_setter(&self, property_name: Symbol, body: &[TypedStmt<'a>]) -> Option<Type> {
        self.find_property_accessor(property_name, body, "setter")
    }

    /// Find property deleter method for a given property name
    fn find_property_deleter(&self, property_name: Symbol, body: &[TypedStmt<'a>]) -> Option<Type> {
        self.find_property_accessor(property_name, body, "deleter")
    }

    /// Generic method to find property accessor (setter/deleter)
    fn find_property_accessor(
        &self,
        property_name: Symbol,
        body: &[TypedStmt<'a>],
        accessor_type: &str,
    ) -> Option<Type> {
        // Look for a method decorated with @property_name.setter or @property_name.deleter
        for stmt in body {
            if let TypedStmt::FuncDef(func) = stmt {
                // Check if any decorator matches the accessor pattern
                for decorator in func.decorators {
                    if let TypedExpr::Attribute(attr) = decorator {
                        // Check if it matches pattern: property_name.accessor_type
                        if let Some(attr_name) = self.interner.resolve(attr.attr)
                            && attr_name == accessor_type
                            && let TypedExpr::Name(base) = &attr.value
                            && base.symbol == property_name
                        {
                            // Found the accessor for the property
                            // Validate signature: setter should have one parameter, deleter should have none
                            if self.validate_accessor_signature(func, accessor_type) {
                                return Some(func.ty.clone());
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Validate property accessor signature
    fn validate_accessor_signature(
        &self,
        func: &super::typed_stmt::TypedFuncDefStmt<'a>,
        accessor_type: &str,
    ) -> bool {
        // Get the parameter count (excluding self)
        let param_count = func.args.args.len().saturating_sub(1);

        match accessor_type {
            "setter" => {
                // Setter should have exactly one parameter (plus self)
                param_count == 1
            }
            "deleter" => {
                // Deleter should have no parameters (just self)
                param_count == 0
            }
            _ => false,
        }
    }

    /// Check if function has @property decorator
    fn has_property_decorator(&self, decorators: &[TypedExpr<'a>]) -> bool {
        for decorator in decorators {
            if let TypedExpr::Name(name_expr) = decorator {
                if let Some("property") = self.interner.resolve(name_expr.symbol) {
                    return true;
                }
            } else if let TypedExpr::Attribute(attr) = decorator
                && let TypedExpr::Name(base) = attr.value
                && let Some("property") = self.interner.resolve(base.symbol)
            {
                // Handle @foo.property style decorators
                return true;
            }
        }
        false
    }

    /// Detect method decorators (@staticmethod, @classmethod, @operator)
    fn detect_method_decorator(&self, decorators: &[TypedExpr<'a>]) -> MethodDecorator {
        for decorator in decorators {
            if let TypedExpr::Name(name_expr) = decorator {
                match self.interner.resolve(name_expr.symbol) {
                    Some("staticmethod") => return MethodDecorator::StaticMethod,
                    Some("classmethod") => return MethodDecorator::ClassMethod,
                    Some("operator") => return MethodDecorator::Operator,
                    _ => {}
                }
            }
        }
        MethodDecorator::None
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
        let interner = Interner::new();
        let mut analyzer = ClassAnalyzer::new(&interner);

        // Simplified unit test for MRO computation
        // Full integration tests use actual parsed class definitions
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
        let interner = Interner::new();
        let mut analyzer = ClassAnalyzer::new(&interner);

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
