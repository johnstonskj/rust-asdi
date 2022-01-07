use asdi::idb::{Evaluator, NaiveEvaluator};
use asdi::io::{print_relation, Format};
use asdi::parse::parse_file;
use asdi::{Collection, Labeled};

fn main() {
    let mut program = parse_file("examples/rdfs.dl").unwrap().into_parsed();

    program.load_extensional_data().unwrap();

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program);
    assert!(results.is_ok());
    let new_relations = results.unwrap();

    for relation in new_relations.iter() {
        println!("{}", relation.label_ref());
        print_relation(relation, &Format::Text).unwrap();
    }
}
