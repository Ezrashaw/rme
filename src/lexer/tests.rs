use crate::{Lexer, SourceMap};
use std::{fs, io::Write, path::PathBuf};

fn get_test_files() -> Vec<PathBuf> {
    let mut directory = PathBuf::from(file!());
    directory.pop();

    directory
        .read_dir()
        .unwrap()
        .filter_map(|d| d.ok().map(|d| d.path()))
        .filter(|d| d.extension().unwrap_or_default() == "test")
        .collect::<Vec<_>>()
}

#[test]
fn lexer_regression_tests() {
    let files = get_test_files();

    for file in files {
        let input = fs::read_to_string(&file).unwrap();
        let lexer = Lexer::new(&input, 0);

        let out_file = file.with_extension("lexed");
        let mut out_file = fs::File::create(out_file).unwrap();
        for tok in lexer {
            match tok {
                Ok(tok) => writeln!(out_file, "{tok:?}").unwrap(),
                Err(err) => {
                    let source_map =
                        SourceMap::from_lines(input.lines().map(ToOwned::to_owned).collect());
                    err.emit_to_write(&mut out_file, &source_map);
                }
            }
        }
    }
}
