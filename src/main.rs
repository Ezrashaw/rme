use rme::{
    parse,
    typeck::{self, TypeEnv},
    SourceMap,
};
use std::io::{stdin, Read};

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();
    let source_map = SourceMap::from_input(input);

    let ast = parse(source_map.source()).unwrap();
    let formatted = ast.to_string();
    print!("{formatted}");

    let mut ty_env = TypeEnv::empty();
    typeck::infer(&mut ty_env, &ast).unwrap();
}
