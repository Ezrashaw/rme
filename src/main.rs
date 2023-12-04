use rme::{
    diag::DErr,
    parse,
    typeck::{self, TypeEnv},
    SourceMap,
};
use std::io::{stdin, Read};

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();
    let source_map = SourceMap::from_input(input);

    match compile(&source_map) {
        Ok(_) => println!("compilation successful"),
        Err(diag) => diag.emit(&source_map),
    }
}

fn compile(source: &SourceMap) -> Result<(), DErr> {
    let ast = parse(source.source())?;
    let formatted = ast.to_string();
    print!("{formatted}");

    let mut ty_env = TypeEnv::empty();
    typeck::infer(&mut ty_env, &ast)?;

    Ok(())
}
