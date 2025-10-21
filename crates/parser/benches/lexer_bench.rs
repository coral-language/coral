use coral_parser::lexer::Lexer;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

fn bench_lexer_simple(c: &mut Criterion) {
    let source = "x = 1 + 2 * 3";
    c.bench_function("lexer_simple", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(source));
            let _ = black_box(lexer.tokenize());
        });
    });
}

fn bench_lexer_keywords(c: &mut Criterion) {
    let source = "if x and y or not z: pass elif a: return else: break";
    c.bench_function("lexer_keywords", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(source));
            let _ = black_box(lexer.tokenize());
        });
    });
}

fn bench_lexer_function(c: &mut Criterion) {
    let source = "def fibonacci(n):\n    if n <= 1:\n        return n\n    return fibonacci(n - 1) + fibonacci(n - 2)\n";
    c.bench_function("lexer_function", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(source));
            let _ = black_box(lexer.tokenize());
        });
    });
}

criterion_group!(
    benches,
    bench_lexer_simple,
    bench_lexer_keywords,
    bench_lexer_function
);
criterion_main!(benches);
