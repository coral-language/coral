use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

pub fn start() {
  println!("🪸 Coral REPL v0.1.0");
  println!("Type 'exit' or 'quit' to exit\n");

  let mut evaluator = Evaluator::new();

  loop {
    print!("coral> ");
    io::stdout().flush().unwrap();

    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
      Ok(_) => {
        let input = input.trim();

        if input.is_empty() {
          continue;
        }

        if input == "exit" || input == "quit" {
          println!("Goodbye! 🪸");
          break;
        }

        // Special commands
        if input == "help" {
          print_help();
          continue;
        }

        if input == "env" {
          println!("{:#?}", evaluator.environment);
          continue;
        }

        // Evaluate input
        let mut lexer = Lexer::new(input.to_string());
        let tokens = match lexer.tokenize() {
          Ok(tokens) => tokens,
          Err(err) => {
            eprintln!("Lex error: {}", err);
            continue;
          }
        };

        let mut parser = Parser::new(tokens);
        match parser.parse() {
          Ok(program) => match evaluator.eval_program(&program) {
            Ok(result) => {
              if result != crate::evaluator::value::Value::None {
                println!("{}", result);
              }
            }
            Err(e) => eprintln!("Runtime error: {}", e),
          },
          Err(e) => eprintln!("Parse error: {}", e),
        }
      }
      Err(e) => {
        eprintln!("Error reading input: {}", e);
        break;
      }
    }
  }
}

fn print_help() {
  println!("Coral REPL Commands:");
  println!("  help  - Show this help message");
  println!("  env   - Show current environment");
  println!("  exit  - Exit the REPL");
  println!("  quit  - Exit the REPL");
  println!("\nYou can type any Coral expression or statement.");
}
