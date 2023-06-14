use rme::{
    ast::Statement,
    lexer::Lexer,
    token::Token,
    ty::TypeVar,
    typeck::{self, Subst, TypeEnv},
    DErr, Parser, SourceMap, Sp,
};
use std::io::{stdin, stdout, Write};

fn main() {
    let mut source_map = SourceMap::new();
    let mut ty_env = TypeEnv::empty();
    let mut vg = typeck::utils::TypeVarGen::new();

    loop {
        let stmt = get_stmt(&mut source_map);
        match stmt {
            Ok(Some(stmt)) => {
                let ty = typeck::infer(&mut ty_env, &mut vg, stmt.inner());
                println!("{ty}");
            }
            Ok(None) => {}
            Err(diag) => diag.emit(&source_map),
        }
    }
}

fn get_stmt(source_map: &mut SourceMap) -> Result<Option<Sp<Statement>>, DErr> {
    print!("$ ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    if input.trim_end().is_empty() {
        return Ok(None);
    }

    let offset = source_map.len();
    source_map.push_line(&input);

    let ast = parse_stmt(&input, offset)?;

    print!("\x1B[1A\x1B[K");
    let formatted = ast.to_string();
    println!("$ {formatted}");

    Ok(Some(ast))
}

fn parse_stmt(input: &str, span_offset: usize) -> Result<Sp<Statement>, DErr> {
    let tokens = Lexer::new(input, span_offset).collect::<Result<Vec<Token>, DErr>>()?;

    let parser = Parser::new(tokens.into_iter());
    parser.parse_single_stmt()
}
