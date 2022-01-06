use asdi::edb::{Constant, Predicate};
use asdi::idb::{Atom, Term, Variable};
use asdi::parse::parse_str;
use std::rc::Rc;
use std::str::FromStr;

#[test]
fn test_matches() {
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

    let query_term = Atom::new(
        human.clone(),
        [Term::Variable(Variable::from_str("X").unwrap())],
    );
    let results = program.extensional().matches(&query_term).unwrap();
    println!("{}", results);

    let query_term = Atom::new(human, [Term::Constant(Constant::from("Socrates"))]);
    let results = program.extensional().matches(&query_term).unwrap();
    println!("{}", results);
}
