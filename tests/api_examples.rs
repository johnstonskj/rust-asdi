use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Term, Variable};
use asdi::program::Program;
use asdi::query::Query;
use std::str::FromStr;

pub mod common;
use common::assert_eq_by_line;

#[test]
fn test_wikipedia_example() {
    // See https://en.wikipedia.org/wiki/Datalog

    let mut ancestors = Program::default();

    let parent_predicate = Predicate::from_str("parent").unwrap();
    {
        let parent = ancestors
            .add_new_relation(
                parent_predicate.clone(),
                vec![Attribute::string(), Attribute::string()],
            )
            .unwrap();
        parent.add(["xerces".into(), "brooke".into()]).unwrap();
        parent.add(["brooke".into(), "damocles".into()]).unwrap();
    };

    let ancestor_predicate = Predicate::from_str("ancestor").unwrap();
    {
        let _ = ancestors
            .add_new_relation(
                ancestor_predicate.clone(),
                vec![Attribute::string(), Attribute::string()],
            )
            .unwrap();
    };

    let var_x: Term = Variable::from_str("X").unwrap().into();
    let var_y: Term = Variable::from_str("Y").unwrap().into();
    let var_z: Term = Variable::from_str("Z").unwrap().into();

    ancestors
        .add_new_rule(
            ancestor_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [Atom::new(parent_predicate.clone(), [var_x.clone(), var_y.clone()]).into()],
        )
        .unwrap();
    ancestors
        .add_new_rule(
            ancestor_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [
                Atom::new(parent_predicate, [var_x.clone(), var_z.clone()]).into(),
                Atom::new(ancestor_predicate.clone(), [var_z, var_y]).into(),
            ],
        )
        .unwrap();

    ancestors
        .add_new_query(ancestor_predicate, ["xerces".into(), var_x])
        .unwrap();

    println!(">{}<", ancestors);

    assert_eq_by_line(
        &ancestors.to_string(),
        r#"@declare ancestor(string, string).
@declare parent(string, string).

parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ ancestor(Z, Y).

?- ancestor(xerces, X).
"#,
    )
}

#[test]
fn test_sourceforge_example() {
    // See http://datalog.sourceforge.net/datalog.html

    let mut graph = Program::default();

    let edge_predicate = Predicate::from_str("edge").unwrap();
    {
        let edge = graph
            .add_new_relation(
                edge_predicate.clone(),
                vec![Attribute::string(), Attribute::string()],
            )
            .unwrap();
        edge.add(["a".into(), "b".into()]).unwrap();
        edge.add(["b".into(), "c".into()]).unwrap();
        edge.add(["c".into(), "d".into()]).unwrap();
        edge.add(["d".into(), "a".into()]).unwrap();
    }

    let path_predicate = Predicate::from_str("path").unwrap();
    {
        let _ = graph
            .add_new_relation(
                path_predicate.clone(),
                vec![Attribute::string(), Attribute::string()],
            )
            .unwrap();
    }

    let var_x: Term = Variable::from_str("X").unwrap().into();
    let var_y: Term = Variable::from_str("Y").unwrap().into();
    let var_z: Term = Variable::from_str("Z").unwrap().into();

    graph
        .add_new_rule(
            path_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [Atom::new(edge_predicate.clone(), [var_x.clone(), var_y.clone()]).into()],
        )
        .unwrap();

    graph
        .add_new_rule(
            path_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [
                Atom::new(edge_predicate, [var_x.clone(), var_z.clone()]).into(),
                Atom::new(path_predicate.clone(), [var_z, var_y.clone()]).into(),
            ],
        )
        .unwrap();

    let query = Query::new(path_predicate, vec![var_x, var_y]);
    graph.add_query(query).unwrap();

    println!("{}", graph);

    assert_eq_by_line(
        &graph.to_string(),
        r#"@declare edge(string, string).
@declare path(string, string).

edge(a, b).
edge(c, d).
edge(d, a).
edge(b, c).

path(X, Y) ⟵ edge(X, Y).
path(X, Y) ⟵ edge(X, Z) ∧ path(Z, Y).

?- path(X, Y).
"#,
    );
}

#[test]
fn test_that_syllogism() {
    // See https://en.wikipedia.org/wiki/Syllogism

    let mut syllogism = Program::default();
    let p_human = Predicate::from_str("human").unwrap();

    let human = syllogism
        .add_new_relation(p_human.clone(), vec![Attribute::string()])
        .unwrap();
    human.add(["Socrates".into()]).unwrap();

    let var_x: Term = Variable::from_str("X").unwrap().into();

    syllogism
        .add_new_rule(
            Predicate::from_str("mortal").unwrap(),
            [var_x.clone()],
            [Atom::new(p_human, [var_x]).into()],
        )
        .unwrap();

    syllogism
        .add_new_query(Predicate::from_str("mortal").unwrap(), ["Socrates".into()])
        .unwrap();

    println!("{}", syllogism);

    assert_eq_by_line(
        &syllogism.to_string(),
        r#"@declare human(string).

human("Socrates").

mortal(X) ⟵ human(X).

?- mortal("Socrates").
"#,
    );
}
