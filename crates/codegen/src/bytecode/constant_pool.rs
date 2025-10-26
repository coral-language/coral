//! Constant pool for bytecode instructions
//!
//! Stores all literals and constants that bytecode instructions reference by ID.
//! This includes:
//! - String literals
//! - Numeric constants
//! - Symbol names
//! - Module references

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantPool {
    constants: IndexMap<u32, Constant>,
    next_id: u32,
    reverse_map: HashMap<String, u32>,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            constants: IndexMap::new(),
            next_id: 0,
            reverse_map: HashMap::new(),
        }
    }

    pub fn add_integer(&mut self, value: i64) -> u32 {
        let key = format!("int:{}", value);
        if let Some(&id) = self.reverse_map.get(&key) {
            return id;
        }

        let id = self.next_id;
        self.next_id += 1;
        self.constants.insert(id, Constant::Integer(value));
        self.reverse_map.insert(key, id);
        id
    }

    pub fn add_float(&mut self, value: f64) -> u32 {
        let key = format!("float:{}", value.to_bits());
        if let Some(&id) = self.reverse_map.get(&key) {
            return id;
        }

        let id = self.next_id;
        self.next_id += 1;
        self.constants.insert(id, Constant::Float(value));
        self.reverse_map.insert(key, id);
        id
    }

    pub fn add_string(&mut self, value: String) -> u32 {
        let key = format!("str:{}", value);
        if let Some(&id) = self.reverse_map.get(&key) {
            return id;
        }

        let id = self.next_id;
        self.next_id += 1;
        self.constants.insert(id, Constant::String(value));
        self.reverse_map.insert(key, id);
        id
    }

    pub fn add_bool(&mut self, value: bool) -> u32 {
        let key = format!("bool:{}", value);
        if let Some(&id) = self.reverse_map.get(&key) {
            return id;
        }

        let id = self.next_id;
        self.next_id += 1;
        self.constants.insert(id, Constant::Bool(value));
        self.reverse_map.insert(key, id);
        id
    }

    pub fn add_none(&mut self) -> u32 {
        let key = "none".to_string();
        if let Some(&id) = self.reverse_map.get(&key) {
            return id;
        }

        let id = self.next_id;
        self.next_id += 1;
        self.constants.insert(id, Constant::None);
        self.reverse_map.insert(key, id);
        id
    }

    pub fn get(&self, id: u32) -> Option<&Constant> {
        self.constants.get(&id)
    }

    pub fn count(&self) -> u32 {
        self.next_id
    }

    pub fn iter(&self) -> impl Iterator<Item = (u32, &Constant)> {
        self.constants.iter().map(|(&id, c)| (id, c))
    }
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_constants() {
        let mut pool = ConstantPool::new();
        let int_id = pool.add_integer(42);
        let str_id = pool.add_string("hello".to_string());
        let bool_id = pool.add_bool(true);

        assert_eq!(pool.count(), 3);
        assert_eq!(pool.get(int_id), Some(&Constant::Integer(42)));
        assert_eq!(
            pool.get(str_id),
            Some(&Constant::String("hello".to_string()))
        );
        assert_eq!(pool.get(bool_id), Some(&Constant::Bool(true)));
    }

    #[test]
    fn test_constant_deduplication() {
        let mut pool = ConstantPool::new();
        let id1 = pool.add_integer(42);
        let id2 = pool.add_integer(42);

        assert_eq!(id1, id2);
        assert_eq!(pool.count(), 1);
    }

    #[test]
    fn test_none_constant() {
        let mut pool = ConstantPool::new();
        let id = pool.add_none();

        assert_eq!(pool.get(id), Some(&Constant::None));
    }
}
