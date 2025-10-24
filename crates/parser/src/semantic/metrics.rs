//! Memory usage monitoring and metrics collection.
//!
//! Provides tools to track memory usage improvements from optimizations
//! like Arc-based symbol table snapshots.

use std::fmt;

/// Memory usage metrics for tracking optimization effectiveness
#[derive(Debug, Clone, Default)]
pub struct MemoryMetrics {
    /// Symbol table size before snapshot creation
    pub symbol_table_size_before: usize,
    /// Symbol table size after snapshot creation (with Arc sharing)
    pub symbol_table_size_after: usize,
    /// Number of Arc references to symbols
    pub arc_reference_count: usize,
    /// Total memory allocated (bytes)
    pub total_allocated_bytes: usize,
    /// Peak memory usage during operation (bytes)
    pub peak_memory_usage: usize,
    /// Number of symbols in the table
    pub symbol_count: usize,
    /// Number of scopes in the symbol table
    pub scope_count: usize,
}

impl MemoryMetrics {
    /// Create new metrics with initial values
    pub fn new() -> Self {
        Self::default()
    }

    /// Calculate memory savings from Arc optimization
    pub fn memory_savings_bytes(&self) -> isize {
        self.symbol_table_size_before as isize - self.symbol_table_size_after as isize
    }

    /// Calculate memory savings as a percentage
    pub fn memory_savings_percentage(&self) -> f64 {
        if self.symbol_table_size_before == 0 {
            0.0
        } else {
            (self.memory_savings_bytes() as f64 / self.symbol_table_size_before as f64) * 100.0
        }
    }

    /// Get average memory per symbol
    pub fn average_memory_per_symbol(&self) -> f64 {
        if self.symbol_count == 0 {
            0.0
        } else {
            self.symbol_table_size_after as f64 / self.symbol_count as f64
        }
    }

    /// Check if metrics indicate memory optimization is working
    pub fn is_optimized(&self) -> bool {
        self.symbol_table_size_after < self.symbol_table_size_before && self.arc_reference_count > 0
    }
}

impl fmt::Display for MemoryMetrics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let savings = self.memory_savings_bytes();
        let savings_pct = self.memory_savings_percentage();
        let avg_memory = self.average_memory_per_symbol();

        writeln!(f, "Memory Metrics:")?;
        writeln!(f, "  Symbol count: {}", self.symbol_count)?;
        writeln!(f, "  Scope count: {}", self.scope_count)?;
        writeln!(
            f,
            "  Symbol table size before: {} bytes",
            self.symbol_table_size_before
        )?;
        writeln!(
            f,
            "  Symbol table size after: {} bytes",
            self.symbol_table_size_after
        )?;
        writeln!(f, "  Arc reference count: {}", self.arc_reference_count)?;
        writeln!(
            f,
            "  Memory savings: {} bytes ({:.1}%)",
            savings, savings_pct
        )?;
        writeln!(f, "  Average memory per symbol: {:.1} bytes", avg_memory)?;
        writeln!(f, "  Total allocated: {} bytes", self.total_allocated_bytes)?;
        writeln!(f, "  Peak memory usage: {} bytes", self.peak_memory_usage)?;
        write!(
            f,
            "  Optimization status: {}",
            if self.is_optimized() {
                "✅ Working"
            } else {
                "❌ Not optimized"
            }
        )
    }
}

/// Configuration for metrics collection
#[derive(Debug, Clone, Default)]
pub struct MetricsConfig {
    /// Enable memory metrics collection
    pub enable_memory_metrics: bool,
    /// Enable detailed symbol tracking
    pub enable_symbol_tracking: bool,
    /// Enable allocation tracking
    pub enable_allocation_tracking: bool,
}

/// Thread-safe metrics collector
#[derive(Debug)]
pub struct MetricsCollector {
    config: MetricsConfig,
    current_metrics: MemoryMetrics,
}

impl MetricsCollector {
    /// Create a new metrics collector
    pub fn new(config: MetricsConfig) -> Self {
        Self {
            config,
            current_metrics: MemoryMetrics::new(),
        }
    }

    /// Reset metrics to zero
    pub fn reset(&mut self) {
        self.current_metrics = MemoryMetrics::new();
    }

    /// Get current metrics
    pub fn current_metrics(&self) -> &MemoryMetrics {
        &self.current_metrics
    }

    /// Update memory metrics
    pub fn update_memory_metrics(&mut self, metrics: MemoryMetrics) {
        if self.config.enable_memory_metrics {
            self.current_metrics = metrics;
        }
    }

    /// Record symbol table snapshot creation
    pub fn record_snapshot_creation(
        &mut self,
        before_size: usize,
        after_size: usize,
        arc_count: usize,
    ) {
        if self.config.enable_memory_metrics {
            self.current_metrics.symbol_table_size_before = before_size;
            self.current_metrics.symbol_table_size_after = after_size;
            self.current_metrics.arc_reference_count = arc_count;
        }
    }

    /// Record symbol count
    pub fn record_symbol_count(&mut self, count: usize) {
        if self.config.enable_symbol_tracking {
            self.current_metrics.symbol_count = count;
        }
    }

    /// Record scope count
    pub fn record_scope_count(&mut self, count: usize) {
        if self.config.enable_symbol_tracking {
            self.current_metrics.scope_count = count;
        }
    }

    /// Record allocation
    pub fn record_allocation(&mut self, bytes: usize) {
        if self.config.enable_allocation_tracking {
            self.current_metrics.total_allocated_bytes += bytes;
            if self.current_metrics.total_allocated_bytes > self.current_metrics.peak_memory_usage {
                self.current_metrics.peak_memory_usage = self.current_metrics.total_allocated_bytes;
            }
        }
    }

    /// Check if memory metrics are enabled
    pub fn memory_metrics_enabled(&self) -> bool {
        self.config.enable_memory_metrics
    }

    /// Check if symbol tracking is enabled
    pub fn symbol_tracking_enabled(&self) -> bool {
        self.config.enable_symbol_tracking
    }

    /// Print current metrics to stdout
    pub fn print_metrics(&self) {
        println!("{}", self.current_metrics);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_metrics_calculations() {
        let mut metrics = MemoryMetrics::new();
        metrics.symbol_table_size_before = 1000;
        metrics.symbol_table_size_after = 600;
        metrics.arc_reference_count = 5;
        metrics.symbol_count = 10;

        assert_eq!(metrics.memory_savings_bytes(), 400);
        assert_eq!(metrics.memory_savings_percentage(), 40.0);
        assert_eq!(metrics.average_memory_per_symbol(), 60.0);
        assert!(metrics.is_optimized());
    }

    #[test]
    fn test_metrics_collector() {
        let config = MetricsConfig {
            enable_memory_metrics: true,
            enable_symbol_tracking: true,
            enable_allocation_tracking: true,
        };

        let mut collector = MetricsCollector {
            config,
            current_metrics: MemoryMetrics::new(),
        };

        collector.record_snapshot_creation(1000, 600, 5);
        collector.record_symbol_count(10);
        collector.record_scope_count(3);
        collector.record_allocation(100);
        collector.record_allocation(50);

        let metrics = collector.current_metrics();
        assert_eq!(metrics.symbol_table_size_before, 1000);
        assert_eq!(metrics.symbol_table_size_after, 600);
        assert_eq!(metrics.arc_reference_count, 5);
        assert_eq!(metrics.symbol_count, 10);
        assert_eq!(metrics.scope_count, 3);
        assert_eq!(metrics.total_allocated_bytes, 150);
        assert_eq!(metrics.peak_memory_usage, 150);
    }

    #[test]
    fn test_display_formatting() {
        let mut metrics = MemoryMetrics::new();
        metrics.symbol_table_size_before = 1000;
        metrics.symbol_table_size_after = 600;
        metrics.arc_reference_count = 5;
        metrics.symbol_count = 10;
        metrics.scope_count = 2;

        let display = format!("{}", metrics);
        assert!(display.contains("Memory Metrics:"));
        assert!(display.contains("Memory savings: 400 bytes (40.0%)"));
        assert!(display.contains("Average memory per symbol: 60.0 bytes"));
        assert!(display.contains("Optimization status: ✅ Working"));
    }
}
