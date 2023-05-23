use rme::{parser::Parser, Diag, DiagEmitter, Interpreter, Lexer, SourceMap, Token};
use std::io::{stdin, stdout, Write};

// FIXME: diagnostics should return an opaque object that can be used as an
//        error in a result

fn main() {
    let mut source_map = SourceMap::new();
    let mut interpreter = Interpreter::new();
    loop {
        let res = get_stmt(&mut source_map, &mut interpreter);

        if let Err(diag) = res {
            diag.emit(&source_map);
        }
    }
}

fn get_stmt(source_map: &mut SourceMap, interpreter: &mut Interpreter) -> Result<(), Diag> {
    print!("> ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    if input.trim().is_empty() {
        return Ok(());
    }

    let offset = source_map.len();

    source_map.push_line(input.clone());
    let emitter = DiagEmitter::new(source_map);

    let lexer = Lexer::new(&input, &emitter, offset);
    let tokens = lexer.collect::<Result<Vec<Token>, Diag>>()?;

    let mut parser = Parser::new(tokens.into_iter(), &emitter);
    let ast = parser.parse()?;

    let formatted = ast.to_string();
    if formatted != input.trim() {
        println!("formatted: `{formatted}`\n");
    }

    interpreter.interpret_stmt(&emitter, ast)?;

    Ok(())
}
