pub mod common;

use asdi::io::csv::Options as CsvOptions;
use asdi::io::json::Options as JsonOptions;
use asdi::io::{print_relation, read_relation, FilePragma, Format};
use asdi::Collection;
use common::make_ancestors;
use std::path::PathBuf;

#[test]
fn read_parents_from_csv_file() {
    let mut program = make_ancestors();

    let label = program.predicates().fetch("parent").unwrap();
    let relation = program.extensional_mut().get_mut(&label).unwrap();

    let new_facts = read_relation(
        relation,
        &FilePragma::new(
            PathBuf::from("tests/data/parents.csv"),
            Format::DelimitedLines(CsvOptions::comma_separated()),
        ),
    )
    .unwrap();

    relation.extend(new_facts).unwrap();
    assert_eq!(relation.len(), 8);

    print_relation(relation, &Format::Text).unwrap();
}

#[test]
fn read_parents_from_csv_file_with_headers() {
    let program = make_ancestors();

    let label = program.predicates().fetch("parent").unwrap();
    let relation = program.extensional().get(&label).unwrap();

    let relation = read_relation(
        relation,
        &FilePragma::new(
            PathBuf::from("tests/data/parents_headered.csv"),
            Format::DelimitedLines(CsvOptions::comma_separated().has_headers()),
        ),
    )
    .unwrap();
    assert_eq!(relation.len(), 6);

    print_relation(&relation, &Format::Text).unwrap();
}

#[test]
fn read_parents_from_json_file() {
    let mut program = make_ancestors();

    let label = program.predicates().fetch("parent").unwrap();
    let relation = program.extensional_mut().get_mut(&label).unwrap();

    let new_facts = read_relation(
        relation,
        &FilePragma::new(
            PathBuf::from("tests/data/parents.json"),
            Format::Json(JsonOptions::default()),
        ),
    )
    .unwrap();

    relation.extend(new_facts).unwrap();
    assert_eq!(relation.len(), 8);

    print_relation(relation, &Format::Text).unwrap();
}
