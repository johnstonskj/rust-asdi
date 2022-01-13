use asdi::edb::Predicate;
use asdi::idb::eval::{Evaluator, NaiveEvaluator};
use asdi::idb::query::{Query, Queryable};
use asdi::parse::parse_str;
use asdi::{Collection, ProgramCore};
use std::str::FromStr;

#[test]
fn test_socrates() {
    const PROGRAM_SOURCE: &str = r#"human("Socrates").
human("Plato").

mortal(X) <- human(X).

?- mortal("Socrates").
"#;

    print!("{}", PROGRAM_SOURCE);
    println!("-------------------------------------------------------------------------------");

    let program = parse_str(PROGRAM_SOURCE).unwrap().into_parsed();

    let p_human = program.predicates().fetch("human").unwrap();
    let p_mortal = program.predicates().fetch("mortal").unwrap();

    assert_eq!(
        program
            .extensional()
            .get(&Predicate::from_str("human").unwrap())
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        program
            .intensional()
            .get(&Predicate::from_str("mortal").unwrap())
            .unwrap()
            .len(),
        0
    );

    print!("{}", program);

    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let new_intensional = evaluator.inference(&program).unwrap();

    assert_eq!(program.extensional().get(&p_human).unwrap().len(), 2);
    assert_eq!(new_intensional.get(&p_mortal).unwrap().len(), 2);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().next().unwrap();

    let results = new_intensional.query(query).unwrap().unwrap();
    println!("{}", results);
    assert_eq!(results.schema().len(), 1);
    assert_eq!(results.len(), 1);

    print!("{} ==>\n{}", query, results);
    println!("-------------------------------------------------------------------------------");

    let query = Query::new(p_mortal, [program.variables().fetch("X").unwrap().into()]);

    let results = new_intensional.query(&query).unwrap().unwrap();
    assert_eq!(results.schema().len(), 1);
    assert_eq!(results.len(), 2);

    print!("{} ==>\n{}", query, results);
}

#[test]
fn test_wikipedia_example() {
    const PROGRAM_SOURCE: &str = r#"parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ parent(Z, Y).

?- ancestor(xerces, X).
"#;

    print!("{}", PROGRAM_SOURCE);
    println!("-------------------------------------------------------------------------------");

    let mut program = parse_str(PROGRAM_SOURCE).unwrap().into_parsed();

    let p_parent = program.predicates().fetch("parent").unwrap();
    let p_ancestor = program.predicates().fetch("ancestor").unwrap();

    assert_eq!(program.extensional().get(&p_parent).unwrap().len(), 2);
    assert_eq!(program.intensional().get(&p_ancestor).unwrap().len(), 0);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program);

    program.intensional_mut().merge(results.unwrap()).unwrap();

    assert_eq!(program.extensional().get(&p_parent).unwrap().len(), 2);
    assert_eq!(program.intensional().get(&p_ancestor).unwrap().len(), 3);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().next().unwrap();

    let results = program.intensional().query(query).unwrap().unwrap();
    println!("{}", results);
    assert_eq!(results.schema().len(), 2);
    assert_eq!(results.len(), 2);

    print!("{} ==>\n{}", query, results);
}

#[test]
fn test_sourceforge_example() {
    const PROGRAM_SOURCE: &str = r#".assert edge(string, string).

edge(a, b).
edge(c, d).
edge(d, a).
edge(b, c).

path(X, Y) ⟵ edge(X, Y).
path(X, Y) ⟵ edge(X, Z) ∧ path(Z, Y).

?- path(X, Y).
"#;

    print!("{}", PROGRAM_SOURCE);
    println!("-------------------------------------------------------------------------------");

    let program = parse_str(PROGRAM_SOURCE).unwrap().into_parsed();

    let p_edge = program.predicates().fetch("edge").unwrap();
    let p_path = program.predicates().fetch("path").unwrap();

    assert_eq!(program.extensional().get(&p_edge).unwrap().len(), 4);
    assert_eq!(program.intensional().get(&p_path).unwrap().len(), 0);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program).unwrap();

    assert_eq!(program.extensional().get(&p_edge).unwrap().len(), 4);
    assert_eq!(results.get(&p_path).unwrap().len(), 16);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().next().unwrap();

    let results = results.query(query).unwrap().unwrap();
    assert_eq!(results.schema().len(), 2);
    assert_eq!(results.len(), 16);

    print!("{} ==>\n{}", query, results);
}
