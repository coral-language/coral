mod error;
mod evaluator;
mod lexer;
mod parser;
mod repl;
mod types;

use error::{CoralError, ErrorKind};
use evaluator::Evaluator;
use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;
use types::checker::TypeChecker;

fn main() {
  let args: Vec<String> = env::args().collect();

  // Start REPL if no arguments
  if args.len() < 2 {
    repl::start();
    return;
  }

  let filename = &args[1];
  let parse_only = args.contains(&"--parse".to_string());
  let skip_typecheck = args.contains(&"--no-typecheck".to_string());

  let source = fs::read_to_string(filename).unwrap_or_else(|_| {
    eprintln!("❌ Failed to read file: {}", filename);
    std::process::exit(1);
  });

  println!("🪸 Compiling {}...\n", filename);

  // Lexing
  let mut lexer = Lexer::new(source);
  let tokens = match lexer.tokenize() {
    Ok(tokens) => tokens,
    Err(err) => {
      eprintln!("\n{}", err.with_file(filename.clone()));
      std::process::exit(1);
    }
  };
  println!("✅ Lexing complete ({} tokens)", tokens.len());

  // Parsing
  let mut parser = Parser::new(tokens);
  let program = match parser.parse() {
    Ok(prog) => {
      println!("✅ Parsing complete ({} statements)", prog.statements.len());
      prog
    }
    Err(e) => {
      eprintln!(
        "\n{}",
        CoralError::new(ErrorKind::Parse, e, 0, 0).with_file(filename.clone())
      );
      std::process::exit(1);
    }
  };

  if parse_only {
    println!("\n🪸 Parse successful!");
    return;
  }

  // Type checking
  if !skip_typecheck {
    let mut type_checker = TypeChecker::new();
    match type_checker.check_program(&program) {
      Ok(_) => println!("✅ Type checking complete"),
      Err(e) => {
        eprintln!(
          "\n{}",
          CoralError::new(ErrorKind::Type, e, 0, 0).with_file(filename.clone())
        );
        std::process::exit(1);
      }
    }
  }

  // Evaluation
  println!("\n🪸 Running...\n");
  let mut evaluator = Evaluator::new();
  match evaluator.eval_program(&program) {
    Ok(result) => {
      println!("\n🪸 Program completed successfully");
      if result != evaluator::value::Value::None {
        println!("Result: {}", result);
      }
    }
    Err(e) => {
      eprintln!(
        "\n{}",
        CoralError::new(ErrorKind::Runtime, e, 0, 0).with_file(filename.clone())
      );
      std::process::exit(1);
    }
  }
}
