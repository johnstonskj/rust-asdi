use asdi::edb::Predicate;
use asdi::idb::{Evaluator, NaiveEvaluator, Query, Variable};
use asdi::parse::parse_str;
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

    assert_eq!(
        program
            .extensional()
            .relation(&Predicate::from_str("human").unwrap())
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        program
            .intensional()
            .relation(&Predicate::from_str("mortal").unwrap())
            .unwrap()
            .len(),
        0
    );

    print!("{}", program);

    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let new_intensional = evaluator.inference(&program).unwrap();

    println!("RESULT: {:#?}", new_intensional);

    assert_eq!(
        program
            .extensional()
            .relation(&Predicate::from_str("human").unwrap())
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        new_intensional
            .relation(&Predicate::from_str("mortal").unwrap())
            .unwrap()
            .len(),
        2
    );

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().next().unwrap();

    let results = new_intensional.matches(query.as_ref()).unwrap();
    assert_eq!(results.schema().arity(), 1);
    assert_eq!(results.len(), 1);

    print!("{} ==>\n{}", query, results);
    println!("-------------------------------------------------------------------------------");

    let query = Query::new(
        Predicate::from_str("mortal").unwrap(),
        [Variable::from_str("X").unwrap().into()],
    );

    let results = new_intensional.matches(query.as_ref()).unwrap();
    assert_eq!(results.schema().arity(), 1);
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

    assert_eq!(
        program
            .extensional()
            .relation(&Predicate::from_str("parent").unwrap())
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        program
            .intensional()
            .relation(&Predicate::from_str("ancestor").unwrap())
            .unwrap()
            .len(),
        0
    );

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program);

    program.intensional_mut().merge(results.unwrap()).unwrap();

    assert_eq!(
        program
            .extensional()
            .relation(&Predicate::from_str("parent").unwrap())
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        program
            .intensional()
            .relation(&Predicate::from_str("ancestor").unwrap())
            .unwrap()
            .len(),
        3
    );

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().next().unwrap();

    let results = program.intensional().matches(query.as_ref()).unwrap();
    assert_eq!(results.schema().arity(), 2);
    assert_eq!(results.len(), 2);

    print!("{} ==>\n{}", query, results);
}

#[test]
fn test_sourceforge_example() {
    const PROGRAM_SOURCE: &str = r#"@assert edge(string, string).

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

    assert_eq!(
        program
            .extensional()
            .relation(&Predicate::from_str("edge").unwrap())
            .unwrap()
            .len(),
        4
    );
    assert_eq!(
        program
            .intensional()
            .relation(&Predicate::from_str("path").unwrap())
            .unwrap()
            .len(),
        0
    );

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program).unwrap();

    assert_eq!(
        program
            .extensional()
            .relation(&Predicate::from_str("edge").unwrap())
            .unwrap()
            .len(),
        4
    );
    assert_eq!(
        results
            .relation(&Predicate::from_str("path").unwrap())
            .unwrap()
            .len(),
        16
    );

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().next().unwrap();

    let results = results.matches(query.as_ref()).unwrap();
    assert_eq!(results.schema().arity(), 2);
    assert_eq!(results.len(), 16);

    print!("{} ==>\n{}", query, results);
}
