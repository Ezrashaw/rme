use rme::{parser::Parser, DErr, Interpreter, Lexer, SourceMap, Token};
use std::io::{stdin, stdout, Write};

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

fn get_stmt(source_map: &mut SourceMap, interpreter: &mut Interpreter) -> Result<(), DErr> {
    print!("$ ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    if input.trim().is_empty() {
        return Ok(());
    }

    let offset = source_map.len();
    source_map.push_line(input.clone());

    let lexer = Lexer::new(&input, offset);
    let tokens = lexer.collect::<Result<Vec<Token>, DErr>>()?;

    let mut parser = Parser::new(tokens.into_iter());
    let ast = parser.parse()?;

    let formatted = ast.to_string();
    if formatted != input.trim() {
        println!("formatted: `{formatted}`");
    }

    let diag = interpreter.interpret_stmt(ast)?;
    if let Some(diag) = diag {
        diag.emit(&source_map);
    }

    Ok(())
}
