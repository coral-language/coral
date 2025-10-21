# Contributing to Coral

Thank you for your interest in contributing to Coral! We're excited to have you join us in building a modern programming language that combines Python's elegance with Rust's safety and performance.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
- [Development Setup](#development-setup)
- [Coding Standards](#coding-standards)
- [Testing Guidelines](#testing-guidelines)
- [Submitting Changes](#submitting-changes)
- [Project Structure](#project-structure)
- [Getting Help](#getting-help)

## Code of Conduct

This project adheres to the Contributor Covenant [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to the project team.

## Getting Started

Coral is currently in active development, with focus on completing the frontend (parser, lexer, semantic analysis) before moving to the backend. Check our [ROADMAP.md](ROADMAP.md) to see what we're working on and where you can help.

### Areas Where We Need Help

- **Parser & Lexer**: Improving error recovery, adding new syntax features
- **Semantic Analysis**: Type checking, symbol resolution, error diagnostics
- **LSP**: Language Server Protocol implementation for IDE support
- **Documentation**: Examples, tutorials, API documentation
- **Testing**: Writing tests, improving test coverage
- **Tooling**: CLI improvements, formatter, benchmarks

## How to Contribute

### Reporting Issues

If you encounter bugs or have suggestions, please use our issue templates:

- **Bugs**: Use the [bug report template](.github/ISSUE_TEMPLATE/bug_report.md)
- **Features**: Use the [feature request template](.github/ISSUE_TEMPLATE/feature_request.md)

When reporting issues, please include:
- Clear description of the problem or suggestion
- Steps to reproduce (for bugs)
- Expected vs. actual behavior
- Relevant code snippets or examples
- Your environment (OS, Rust version)

### Proposing Major Changes

For significant changes or new features:
1. Open an issue first to discuss your proposal
2. Wait for maintainer feedback before starting work
3. Reference the issue in your pull request

## Development Setup

### Prerequisites

- **Rust**: 1.90 or later (specified in `rust-toolchain.toml`)
- **Git**: For version control
- **Cargo**: Comes with Rust

### Setting Up Your Development Environment

1. **Fork the repository** on GitHub

2. **Clone your fork**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/coral.git
   cd coral
   ```

3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/coral-language/coral.git
   ```

4. **Build the project**:
   ```bash
   cargo build
   ```

5. **Run tests**:
   ```bash
   cargo test
   ```

6. **Run the CLI** (if applicable):
   ```bash
   cargo run -- --help
   ```

## Coding Standards

### Rust Style Guidelines

- Follow standard Rust conventions and idioms
- Run `cargo fmt` before committing to ensure consistent formatting
- Run `cargo clippy` and address any warnings
- Use meaningful variable and function names
- Add documentation comments (`///`) for public APIs
- Keep functions focused and reasonably sized

### Commit Message Guidelines

We follow conventional commit format:

```
<type>(<scope>): <subject>

<body>

<footer>
```

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks
- `perf`: Performance improvements

**Examples**:
```
feat(parser): add support for async functions

Implements parsing for async/await syntax including:
- async function declarations
- await expressions
- proper error handling

Closes #123
```

```
fix(lexer): handle unicode identifiers correctly

Previously, non-ASCII characters in identifiers caused
lexer panics. Now properly validates and accepts
unicode identifiers per specification.
```

## Testing Guidelines

- **Write tests** for all new features and bug fixes
- **Run tests** locally before submitting: `cargo test`
- **Run specific test suites**:
  - Parser tests: `cargo test -p coral_parser`
  - Lexer benchmarks: `cargo bench -p coral_parser`
- **Add integration tests** for end-to-end scenarios
- **Update tests** when changing existing behavior
- Aim for meaningful test coverage, not just high percentages

### Running Benchmarks

```bash
cargo bench
```

## Submitting Changes

### Pull Request Process

1. **Create a feature branch**:
   ```bash
   git checkout -b feat/your-feature-name
   ```

2. **Make your changes**:
   - Write clean, well-documented code
   - Add tests for your changes
   - Update documentation if needed

3. **Ensure quality**:
   ```bash
   cargo fmt
   cargo clippy
   cargo test
   ```

4. **Commit your changes**:
   ```bash
   git add .
   git commit -m "feat(scope): description"
   ```

5. **Keep your branch updated**:
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

6. **Push to your fork**:
   ```bash
   git push origin feat/your-feature-name
   ```

7. **Open a pull request**:
   - Use our [PR template](.github/pull_request_template.md)
   - Reference any related issues
   - Provide clear description of changes
   - Explain why the change is needed

### PR Review Process

- Maintainers will review your PR as soon as possible
- Address any feedback or requested changes
- Keep the PR focused on a single concern
- Be patient and respectful during the review process
- Once approved, a maintainer will merge your PR

## Project Structure

```
coral/
├── crates/
│   ├── parser/      # Lexer, parser, AST, semantic analysis
│   ├── codegen/     # Code generation (bytecode, LLVM)
│   ├── interpreter/ # Interpreter implementation
│   ├── cli/         # Command-line interface
│   └── lsp/         # Language Server Protocol
├── docs/            # Project documentation
├── examples/        # Example Coral programs
└── tests/           # Integration tests
```

### Key Crates

- **`coral_parser`**: Handles lexing, parsing, AST construction, and semantic analysis
- **`coral_codegen`**: Code generation backends
- **`coral_interpreter`**: Interpreter runtime
- **`coral_cli`**: CLI tool for compiling/running Coral programs
- **`coral_lsp`**: LSP implementation for editor support

## Getting Help

- **Questions?** Open a discussion on GitHub Discussions
- **Stuck?** Ask in your PR or issue
- **Chat**: Join our community (links coming soon)

## Recognition

Contributors will be acknowledged in:
- Project README
- Release notes for significant contributions
- Our contributors page (coming soon)

## License

By contributing to Coral, you agree that your contributions will be licensed under the Apache-2.0 License.

---

Thank you for contributing to Coral! Together, we're building something amazing. 🐍✨
