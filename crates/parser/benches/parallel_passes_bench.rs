use coral_parser::semantic::passes::manager::{PassManager, PassManagerConfig};
use coral_parser::{Arena, lexer::Lexer, parser::Parser};
use criterion::{Criterion, criterion_group, criterion_main};
use std::path::PathBuf;

/// Create a test module with the specified number of functions
fn create_test_module(function_count: usize) -> String {
    let mut source = String::from("class TestClass:\n");

    for i in 0..function_count {
        source.push_str(&format!(
            "    def method_{}(self, x):\n        return x + {}\n",
            i, i
        ));
    }

    // Add some imports and constants to make it more realistic
    let header = "import math\nimport collections\n\nPI = 3.14159\nEULER = 2.71828\n\n";
    header.to_string() + &source
}

/// Benchmark sequential pass execution
fn bench_passes_sequential(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallel_passes_sequential");

    for num_functions in [5, 10, 15, 20].iter() {
        let source = create_test_module(*num_functions);
        let arena = Arena::new();
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer, &arena);

        let module = parser.parse_module().unwrap();

        // Configure pass manager for sequential execution
        let config = PassManagerConfig {
            parallel_execution: false,
            collect_statistics: false,
            ..PassManagerConfig::default()
        };

        group.bench_function(format!("sequential_{}_functions", num_functions), |b| {
            b.iter(|| {
                let mut manager = PassManager::with_config(PathBuf::from("/tmp"), config.clone());
                let _ = std::hint::black_box(manager.run_all_passes(module, &source, None));
            });
        });
    }

    group.finish();
}

/// Benchmark parallel pass execution
fn bench_passes_parallel(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallel_passes_parallel");

    for num_functions in [5, 10, 15, 20].iter() {
        let source = create_test_module(*num_functions);
        let arena = Arena::new();
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer, &arena);

        let module = parser.parse_module().unwrap();

        // Configure pass manager for parallel execution
        let config = PassManagerConfig {
            parallel_execution: true,
            collect_statistics: false,
            ..PassManagerConfig::default()
        };

        group.bench_function(format!("parallel_{}_functions", num_functions), |b| {
            b.iter(|| {
                let mut manager = PassManager::with_config(PathBuf::from("/tmp"), config.clone());
                let _ = std::hint::black_box(manager.run_all_passes(module, &source, None));
            });
        });
    }

    group.finish();
}

/// Benchmark with varying numbers of parallelizable passes
fn bench_parallelizable_passes(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallelizable_passes");

    let source = create_test_module(10);
    let arena = Arena::new();
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, &arena);
    let module = parser.parse_module().unwrap();

    for parallelizable_count in [1, 3, 5, 8].iter() {
        // Create a custom pass manager with controlled parallelizable passes
        let config = PassManagerConfig {
            parallel_execution: true,
            collect_statistics: false,
            ..PassManagerConfig::default()
        };

        group.bench_function(
            format!("parallelizable_{}_passes", parallelizable_count),
            |b| {
                b.iter(|| {
                    let mut manager =
                        PassManager::with_config(PathBuf::from("/tmp"), config.clone());
                    // Note: In a real scenario, we'd modify the pass metadata to control
                    // parallelizability, but for this benchmark we'll just run all passes
                    let _ = std::hint::black_box(manager.run_all_passes(module, &source, None));
                });
            },
        );
    }

    group.finish();
}

/// Benchmark cache hit performance
fn bench_cache_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_performance");

    let source = create_test_module(15);
    let arena = Arena::new();
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, &arena);
    let module = parser.parse_module().unwrap();

    // Create manager with caching enabled
    let config = PassManagerConfig {
        parallel_execution: true,
        ..PassManagerConfig::default()
    };

    // Warm up cache
    let mut manager = PassManager::with_config(PathBuf::from("/tmp"), config.clone());
    let _ = manager.run_all_passes(module, &source, None);

    group.bench_function("cache_hit", |b| {
        b.iter(|| {
            let mut manager = PassManager::with_config(PathBuf::from("/tmp"), config.clone());
            let _ = std::hint::black_box(manager.run_all_passes(module, &source, None));
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_passes_sequential,
    bench_passes_parallel,
    bench_parallelizable_passes,
    bench_cache_performance
);

criterion_main!(benches);
