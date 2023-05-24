use rme::{lex, parser::Parser, DErr, Interpreter, SourceMap, Token};
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

    if input.trim_end().is_empty() {
        return Ok(());
    }

    let offset = source_map.len();
    source_map.push_line(input.clone());

    let tokens = lex(&input, offset)
        .into_iter()
        .collect::<Result<Vec<Token>, DErr>>()?;

    let parser = Parser::new(tokens.into_iter());
    let ast = parser.parse()?;

    let formatted = ast.to_string();
    if formatted != input.trim_end() {
        println!("formatted: `{formatted}`");
    }

    let diag = interpreter.interpret_stmt(ast)?;
    if let Some(diag) = diag {
        diag.emit(source_map);
    }

    Ok(())
}
