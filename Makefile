.PHONY: help build test clean install fmt lint run repl examples docs release

# Default target
help:
	@echo "🪸 Coral Programming Language - Build Commands"
	@echo ""
	@echo "  make build      - Build the project"
	@echo "  make test       - Run tests"
	@echo "  make clean      - Clean build artifacts"
	@echo "  make install    - Install coral binary"
	@echo "  make fmt        - Format code"
	@echo "  make lint       - Run clippy linter"
	@echo "  make run        - Start REPL"
	@echo "  make examples   - Run all examples"
	@echo "  make docs       - Generate documentation"
	@echo "  make release    - Build optimized release"
	@echo "  make check      - Run all checks (fmt, lint, test)"

build:
	@echo "🔨 Building Coral..."
	cargo build

test:
	@echo "🧪 Running tests..."
	cargo test --verbose

clean:
	@echo "🧹 Cleaning build artifacts..."
	cargo clean

install:
	@echo "📦 Installing coral..."
	cargo install --path .

fmt:
	@echo "🎨 Formatting code..."
	cargo fmt --all

lint:
	@echo "🔍 Running clippy..."
	cargo clippy -- -D warnings

run:
	@echo "🪸 Starting Coral REPL..."
	cargo run --release

examples:
	@echo "📚 Running examples..."
	@for file in examples/*.coral; do \
		echo "Running $$file..."; \
		cargo run --release "$$file"; \
		echo ""; \
	done

docs:
	@echo "📖 Generating documentation..."
	cargo doc --no-deps --open

release:
	@echo "🚀 Building release version..."
	cargo build --release
	@echo "✅ Binary available at: target/release/coral"

check: fmt lint test
	@echo "✅ All checks passed!"

# Benchmarking (requires cargo-criterion)
bench:
	@echo "⚡ Running benchmarks..."
	cargo bench

# Code coverage (requires cargo-tarpaulin)
coverage:
	@echo "📊 Generating coverage report..."
	cargo tarpaulin --out Html --output-dir coverage

# Watch mode (requires cargo-watch)
watch:
	@echo "👀 Starting watch mode..."
	cargo watch -x check -x test -x run