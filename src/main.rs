use rme::{parser::Parser, DiagEmitter, Interpreter, Lexer, SourceMap, Token};
use std::io::{stdin, stdout, Write};

// FIXME: diagnostics should return an opaque object that can be used as an
//        error in a result

fn main() {
    let mut source_map = SourceMap::new();
    let mut interpreter = Interpreter::new();
    loop {
        get_stmt(&mut source_map, &mut interpreter)
    }
}

fn get_stmt(source_map: &mut SourceMap, interpreter: &mut Interpreter) {
    print!("> ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    if input.trim().is_empty() {
        return;
    }

    let offset = source_map.len();

    source_map.push_line(input.clone());
    let emitter = DiagEmitter::new(source_map);

    let lexer = Lexer::new(&input, &emitter, offset);
    let Some(tokens) = lexer.collect::<Option<Vec<Token>>>() else {
        return;
    };

    let parser = Parser::new(tokens.into_iter(), &emitter);
    let Some(ast) = parser.parse() else {
        return;
    };

    let formatted = ast.to_string();
    if formatted != input.trim() {
        println!("formatted: `{formatted}`\n");
    }

    interpreter.interpret_stmt(&emitter, ast);
}
