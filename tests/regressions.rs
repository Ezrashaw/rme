#![feature(test)]
extern crate test;

use lexer::LexerTests;
use parser::ParserTests;
use std::{
    env,
    error::Error,
    fs,
    io::{self, Write},
    path::PathBuf,
    process::{Command, ExitCode},
};
use test::{
    ColorConfig, Options, OutputFormat, RunIgnored, ShouldPanic, TestDesc, TestDescAndFn, TestFn,
    TestOpts, TestType,
};

mod lexer;
mod parser;

pub trait RegressionTests {
    const NAMESPACE: &'static str;

    fn tests_directory() -> PathBuf {
        PathBuf::from(file!()).with_file_name(Self::NAMESPACE)
    }

    fn run_test(out: &mut impl Write, input: &str) -> io::Result<()>;
}

fn main() -> ExitCode {
    let mut tests = Vec::new();
    generate_tests::<LexerTests>(&mut tests).unwrap();
    generate_tests::<ParserTests>(&mut tests).unwrap();

    let success = test::run_tests_console(&TEST_OPTIONS, tests).unwrap();

    if success {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn get_ext_files(directory: PathBuf, extension: &str) -> Result<Vec<PathBuf>, io::Error> {
    Ok(directory
        .read_dir()?
        .filter_map(|d| d.ok().map(|d| d.path()))
        .flat_map(|d| {
            if d.is_dir() {
                get_ext_files(d, extension).unwrap()
            } else {
                vec![d]
            }
        })
        .filter(|d| d.extension().unwrap_or_default() == extension)
        .collect::<Vec<_>>())
}

fn generate_tests<T: RegressionTests>(
    tests: &mut Vec<TestDescAndFn>,
) -> Result<(), Box<dyn Error>> {
    for file in get_ext_files(T::tests_directory(), "actual")? {
        fs::remove_file(file).unwrap();
    }

    let mut current_dir = PathBuf::from(file!()).canonicalize()?;
    current_dir.pop();
    let current_dir = current_dir;

    for file in get_ext_files(T::tests_directory(), "test")? {
        let blessed = env::args().any(|x| x == "--bless");

        let mut test_name = file.canonicalize()?;
        test_name.set_extension("");
        let test_name = test_name
            .strip_prefix(&current_dir)?
            .to_string_lossy()
            .replace('/', "::");

        tests.push(TestDescAndFn {
            desc: TestDesc {
                name: test::TestName::DynTestName(test_name),
                ignore: false,
                ignore_message: None,
                source_file: Box::leak(file.display().to_string().into_boxed_str()),
                start_line: 0,
                start_col: 0,
                end_line: 0,
                end_col: 0,
                should_panic: ShouldPanic::No,
                compile_fail: false,
                no_run: false,
                test_type: TestType::UnitTest,
            },
            testfn: TestFn::DynTestFn(Box::new(move || run_test::<T>(file, blessed))),
        })
    }

    Ok(())
}

fn run_test<T: RegressionTests>(test_file: PathBuf, bless: bool) -> Result<(), String> {
    let input = fs::read_to_string(&test_file).unwrap();
    let mut output = Vec::new();
    T::run_test(&mut output, &input).map_err(|e| e.to_string())?;

    let out_file = test_file.with_extension("out");

    if bless || !out_file.exists() {
        fs::write(&out_file, output).unwrap();
        return Ok(());
    }

    let current_output = fs::read(&out_file).unwrap();

    if output != current_output {
        let actual_file = out_file.with_extension("actual");
        fs::write(&actual_file, output).unwrap();

        let diff = Command::new("diff")
            .arg(out_file)
            .arg(actual_file)
            .arg("-U9999")
            .arg("--color=always")
            .output()
            .unwrap();

        let diff = String::from_utf8(diff.stdout).unwrap();
        let diff = diff
            .lines()
            .skip(3)
            .map(|x| format!("{x}\n"))
            .collect::<String>();

        let display = test_file.display();

        print!("\x1B[3;34m---{display}---\x1B[0m\n{diff}");
        println!(
            "\x1B[34m{:-^width$}\x1B[0m",
            "",
            width = display.to_string().len() + 6
        );
        return Err("new output did not match saved output".to_owned());
    }

    Ok(())
}

const TEST_OPTIONS: TestOpts = TestOpts {
    list: false,
    filters: Vec::new(),
    filter_exact: false,
    force_run_in_process: false,
    exclude_should_panic: false,
    run_ignored: RunIgnored::No,
    run_tests: true,
    bench_benchmarks: false,
    logfile: None,
    nocapture: false,
    color: ColorConfig::AlwaysColor,
    format: OutputFormat::Pretty,
    shuffle: false,
    shuffle_seed: None,
    test_threads: None,
    skip: Vec::new(),
    time_options: None,
    fail_fast: false,
    options: Options {
        display_output: false,
        panic_abort: false,
    },
};
