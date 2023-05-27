use rme::{parser::parse, DErr, Interpreter, SourceMap, SubDiag, SubDiagLevel};
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

    let ast = parse(&input, offset)?;
    let ast_span = ast.span();

    let formatted = ast.to_string();
    if formatted != input.trim_end() {
        println!("formatted: `{formatted}`");
    }

    let val = interpreter.interpret_stmt(ast)?;

    let mut info_diag = SubDiag::new(
        SubDiagLevel::Info,
        format!("evalulated standalone expression to be `{val}`"),
        ast_span,
    );

    info_diag.add_subdiag(SubDiag::without_span(
        SubDiagLevel::Help,
        "consider using the `print` function",
    ));

    info_diag.emit(source_map);

    Ok(())
}
