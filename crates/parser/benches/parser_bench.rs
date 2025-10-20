use coral_parser::{Arena, lexer::Lexer, parser::Parser};
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

fn bench_parser_simple(c: &mut Criterion) {
    let source = "x = 1 + 2 * 3";

    c.bench_function("parser_simple_expr", |b| {
        b.iter(|| {
            let arena = Box::leak(Box::new(Arena::new()));
            let lexer = Lexer::new(black_box(source));
            let mut parser = Parser::new(lexer, arena);
            let _ = black_box(parser.parse_module());
        });
    });
}

fn bench_parser_function(c: &mut Criterion) {
    let source = r#"
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)
"#;

    c.bench_function("parser_function_def", |b| {
        b.iter(|| {
            let arena = Box::leak(Box::new(Arena::new()));
            let lexer = Lexer::new(black_box(source));
            let mut parser = Parser::new(lexer, arena);
            let _ = black_box(parser.parse_module());
        });
    });
}

fn bench_parser_class(c: &mut Criterion) {
    let source = r#"
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
"#;

    c.bench_function("parser_class_def", |b| {
        b.iter(|| {
            let arena = Box::leak(Box::new(Arena::new()));
            let lexer = Lexer::new(black_box(source));
            let mut parser = Parser::new(lexer, arena);
            let _ = black_box(parser.parse_module());
        });
    });
}

fn bench_parser_large_file(c: &mut Criterion) {
    let source = r#"
class Calculator:
    def __init__(self):
        self.result = 0

    def add(self, x, y):
        return x + y

    def subtract(self, x, y):
        return x - y

    def multiply(self, x, y):
        return x * y

    def divide(self, x, y):
        if y == 0:
            raise ValueError("Cannot divide by zero")
        return x / y

def main():
    calc = Calculator()
    print(calc.add(5, 3))
    print(calc.subtract(10, 4))
"#;

    c.bench_function("parser_large_file", |b| {
        b.iter(|| {
            let arena = Box::leak(Box::new(Arena::new()));
            let lexer = Lexer::new(black_box(source));
            let mut parser = Parser::new(lexer, arena);
            let _ = black_box(parser.parse_module());
        });
    });
}

criterion_group!(
    benches,
    bench_parser_simple,
    bench_parser_function,
    bench_parser_class,
    bench_parser_large_file
);
criterion_main!(benches);
