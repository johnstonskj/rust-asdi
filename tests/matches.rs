use asdi::edb::{Constant, Predicate};
use asdi::idb::query::Queryable;
use asdi::idb::{Atom, Term};
use asdi::parse::parse_str;
use asdi::ProgramCore;
use std::rc::Rc;
use std::str::FromStr;

#[test]
fn test_simple_query() {
    let program = parse_str(
        r#"% --> .assert human(string).
            
human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
"#,
    )
    .unwrap()
    .into_parsed();

    println!("{:#?}", program);
    println!("{}", program.to_string());

    let human = Rc::new(Predicate::from_str("human").unwrap());

    let query_atom = Atom::new(
        human.clone(),
        [Term::Variable(program.variables().fetch("X").unwrap())],
    );
    let results = program
        .extensional()
        .query_atom(&query_atom)
        .unwrap()
        .unwrap();
    println!("{}", results);

    let query_atom = Atom::new(human, [Term::Constant(Constant::from("Socrates"))]);
    let results = program
        .extensional()
        .query_atom(&query_atom)
        .unwrap()
        .unwrap();
    println!("{}", results);
}
