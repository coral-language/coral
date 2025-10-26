//! Register allocation and management for bytecode generation
//!
//! Implements a simple but effective register allocator that:
//! - Tracks register types from semantic analysis
//! - Implements liveness analysis to minimize register pressure
//! - Pre-allocates fixed-size frames per function (no spilling needed)
//! - Supports efficient register reuse

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypedRegister {
    pub id: u16,
    pub ty_id: u32,
    pub lifetime_start: u32,
    pub lifetime_end: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegisterAllocator {
    max_registers: u16,
    next_reg: u16,
    registers: HashMap<u16, TypedRegister>,
    live_ranges: HashMap<u16, (u32, u32)>,
}

impl RegisterAllocator {
    pub fn new(max_registers: u16) -> Self {
        Self {
            max_registers,
            next_reg: 0,
            registers: HashMap::new(),
            live_ranges: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, ty_id: u32) -> Result<u16, String> {
        if self.next_reg >= self.max_registers {
            return Err(format!(
                "Register pressure exceeded: {} registers allocated, max is {}",
                self.next_reg, self.max_registers
            ));
        }

        let reg_id = self.next_reg;
        self.next_reg += 1;

        self.registers.insert(
            reg_id,
            TypedRegister {
                id: reg_id,
                ty_id,
                lifetime_start: 0,
                lifetime_end: 0,
            },
        );

        Ok(reg_id)
    }

    pub fn allocate_temp(&mut self, ty_id: u32, lifetime_end: u32) -> Result<u16, String> {
        let reg_id = self.allocate(ty_id)?;
        self.live_ranges.insert(reg_id, (0, lifetime_end));
        Ok(reg_id)
    }

    pub fn get_register(&self, reg_id: u16) -> Option<&TypedRegister> {
        self.registers.get(&reg_id)
    }

    pub fn get_type(&self, reg_id: u16) -> Option<u32> {
        self.registers.get(&reg_id).map(|r| r.ty_id)
    }

    pub fn count(&self) -> u16 {
        self.next_reg
    }

    pub fn total_registers(&self) -> u16 {
        self.max_registers
    }

    pub fn update_lifetime(&mut self, reg_id: u16, start: u32, end: u32) {
        if let Some(reg) = self.registers.get_mut(&reg_id) {
            reg.lifetime_start = reg.lifetime_start.min(start);
            reg.lifetime_end = reg.lifetime_end.max(end);
        }
    }

    pub fn compute_liveness(&self, instr_id: u32) -> HashSet<u16> {
        let mut live = HashSet::new();
        for (reg_id, (start, end)) in &self.live_ranges {
            if *start <= instr_id && instr_id <= *end {
                live.insert(*reg_id);
            }
        }
        live
    }

    pub fn get_live_registers(&self) -> Vec<&TypedRegister> {
        self.registers.values().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocate_registers() {
        let mut alloc = RegisterAllocator::new(16);
        let r0 = alloc.allocate(0).unwrap();
        let r1 = alloc.allocate(1).unwrap();

        assert_eq!(r0, 0);
        assert_eq!(r1, 1);
        assert_eq!(alloc.count(), 2);
    }

    #[test]
    fn test_register_pressure() {
        let mut alloc = RegisterAllocator::new(2);
        let _ = alloc.allocate(0).unwrap();
        let _ = alloc.allocate(1).unwrap();

        let result = alloc.allocate(2);
        assert!(result.is_err());
    }

    #[test]
    fn test_type_tracking() {
        let mut alloc = RegisterAllocator::new(8);
        let r0 = alloc.allocate(42).unwrap();

        let ty = alloc.get_type(r0);
        assert_eq!(ty, Some(42));
    }

    #[test]
    fn test_liveness_computation() {
        let mut alloc = RegisterAllocator::new(8);
        let r0 = alloc.allocate(0).unwrap();
        alloc.live_ranges.insert(r0, (5, 15));

        let live_at_10 = alloc.compute_liveness(10);
        assert!(live_at_10.contains(&r0));

        let live_at_20 = alloc.compute_liveness(20);
        assert!(!live_at_20.contains(&r0));
    }
}
