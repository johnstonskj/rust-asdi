use asdi::io::csv::Options as CsvOptions;
use asdi::io::json::Options as JsonOptions;
use asdi::io::{print_relation, Format};
use asdi::{Collection, ProgramCore};

pub mod common;
use common::make_and_evaluate_ancestors;

#[test]
fn test_print_as_csv() {
    let program = make_and_evaluate_ancestors();

    for relation in program.intensional().iter() {
        print_relation(
            relation,
            &Format::DelimitedLines(CsvOptions::comma_separated().no_headers()),
        )
        .unwrap();
    }
}

#[test]
fn test_print_as_json() {
    let program = make_and_evaluate_ancestors();

    for relation in program.intensional().iter() {
        print_relation(relation, &Format::Json(JsonOptions::pretty_printed())).unwrap();
    }
}

#[test]
fn test_print_as_text() {
    let program = make_and_evaluate_ancestors();

    for relation in program.intensional().iter() {
        print_relation(relation, &Format::Text).unwrap();
    }
}
