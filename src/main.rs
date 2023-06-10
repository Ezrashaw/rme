use rme::{ast::Statement, DErr, Interpreter, Lexer, Parser, SourceMap, Sp, Token};
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
    source_map.push_line(&input);

    let ast = parse_stmt(&input, offset)?;

    print!("\x1B[1A\x1B[K");
    let formatted = ast.to_string();
    println!("$ {formatted}");

    interpreter.interpret_stmt(ast)?;

    Ok(())
}

fn parse_stmt(input: &str, span_offset: usize) -> Result<Sp<Statement>, DErr> {
    let tokens = Lexer::new(input, span_offset).collect::<Result<Vec<Token>, DErr>>()?;

    let parser = Parser::new(tokens.into_iter());
    parser.parse_single_stmt()
}
