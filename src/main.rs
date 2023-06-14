use rme::{
    ast::Statement, lexer::Lexer, token::Token, ty::PrimType, typeck, DErr, Interpreter, Parser,
    SourceMap, Sp,
};
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

    let ty = typeck::infer_stmt(ast.inner());
    println!("{ty:?}");

    // let poly = PolyType::new(
    //     vec![0, 1],
    //     rme::ty::Type::Function(
    //         vec![PrimType::Bool.into()],
    //         Box::new(PrimType::Float.into()),
    //     ),
    // );
    // println!("{poly}");

    // interpreter.interpret_stmt(ast)?;

    Ok(())
}

fn parse_stmt(input: &str, span_offset: usize) -> Result<Sp<Statement>, DErr> {
    let tokens = Lexer::new(input, span_offset).collect::<Result<Vec<Token>, DErr>>()?;

    let parser = Parser::new(tokens.into_iter());
    parser.parse_single_stmt()
}
