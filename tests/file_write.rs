use asdi::io::csv::Options as CsvOptions;
use asdi::io::json::Options as JsonOptions;
use asdi::io::{write_relation, FilePragma, Format};
use asdi::Collection;
use std::path::PathBuf;

pub mod common;
use common::make_and_evaluate_ancestors;

const TMP_DIR: &str = env!("CARGO_TARGET_TMPDIR");

#[test]
fn test_write_as_csv() {
    let program = make_and_evaluate_ancestors();

    let out_file = PathBuf::from(TMP_DIR).join("write-relation-test.csv");
    println!("{:?}", out_file);

    for relation in program.intensional().iter() {
        write_relation(
            relation,
            FilePragma::output(
                relation,
                out_file.clone(),
                Format::DelimitedLines(CsvOptions::comma_separated()),
            ),
        )
        .unwrap();
    }
}

#[test]
fn test_write_as_text() {
    let program = make_and_evaluate_ancestors();

    let out_file = PathBuf::from(TMP_DIR).join("write-relation-test.txt");
    println!("{:?}", out_file);

    for relation in program.intensional().iter() {
        write_relation(
            relation,
            FilePragma::output(relation, out_file.clone(), Format::Text),
        )
        .unwrap();
    }
}

#[test]
fn test_write_as_json() {
    let program = make_and_evaluate_ancestors();

    let out_file = PathBuf::from(TMP_DIR).join("write-relation-test.json");
    println!("{:?}", out_file);

    for relation in program.intensional().iter() {
        write_relation(
            relation,
            FilePragma::output(
                relation,
                out_file.clone(),
                Format::Json(JsonOptions::plain_output()),
            ),
        )
        .unwrap();
    }
}

#[test]
fn test_write_as_pretty_json() {
    let program = make_and_evaluate_ancestors();

    let out_file = PathBuf::from(TMP_DIR).join("write-relation-test.jsonpp");
    println!("{:?}", out_file);

    for relation in program.intensional().iter() {
        write_relation(
            relation,
            FilePragma::output(
                relation,
                out_file.clone(),
                Format::Json(JsonOptions::pretty_printed()),
            ),
        )
        .unwrap();
    }
}
