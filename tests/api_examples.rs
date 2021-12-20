use asdi::edb::{AttributeKind, Predicate};
use asdi::idb::{Atom, Term, Variable};
use asdi::query::Query;
use asdi::Program;
use std::str::FromStr;

pub mod common;
use common::assert_eq_by_line;

#[test]
fn test_wikipedia_example() {
    // See https://en.wikipedia.org/wiki/Datalog

    let mut ancestors = Program::default();

    let mut parent = ancestors
        .make_new_relation(
            Predicate::from_str("parent").unwrap(),
            [AttributeKind::String, AttributeKind::String],
        )
        .unwrap();
    parent.add(["xerces".into(), "brooke".into()]);
    parent.add(["brooke".into(), "damocles".into()]);

    let ancestor = ancestors
        .make_new_relation(
            Predicate::from_str("ancestor").unwrap(),
            [AttributeKind::String, AttributeKind::String],
        )
        .unwrap();

    let var_x: Term = Variable::from_str("X").unwrap().into();
    let var_y: Term = Variable::from_str("Y").unwrap().into();
    let var_z: Term = Variable::from_str("Z").unwrap().into();

    ancestors
        .add_new_rule(
            ancestor.predicate().clone(),
            [var_x.clone(), var_y.clone()],
            [Atom::new(parent.predicate().clone(), [var_x.clone(), var_y.clone()]).into()],
        )
        .unwrap();
    ancestors
        .add_new_rule(
            ancestor.predicate().clone(),
            [var_x.clone(), var_y.clone()],
            [
                Atom::new(parent.predicate().clone(), [var_x.clone(), var_z.clone()]).into(),
                Atom::new(ancestor.predicate().clone(), [var_z, var_y]).into(),
            ],
        )
        .unwrap();

    ancestors
        .add_new_query(ancestor.predicate().clone(), ["xerces".into(), var_x])
        .unwrap();

    ancestors.add_relation(parent);
    ancestors.add_relation(ancestor);

    println!(">{}<", ancestors);

    assert_eq_by_line(
        &ancestors.to_string(),
        r#"@declare ancestor(string, string).
@declare parent(string, string).

parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ ancestor(Z, Y).

?- ancestor(xerces, X).
"#,
    )
}

#[test]
fn test_sourceforge_example() {
    // See http://datalog.sourceforge.net/datalog.html

    let mut graph = Program::default();

    let mut edge = graph
        .make_new_relation(
            Predicate::from_str("edge").unwrap(),
            [AttributeKind::String, AttributeKind::String],
        )
        .unwrap();
    edge.add(["a".into(), "b".into()]);
    edge.add(["b".into(), "c".into()]);
    edge.add(["c".into(), "d".into()]);
    edge.add(["d".into(), "a".into()]);

    let path = graph
        .make_new_relation(
            Predicate::from_str("path").unwrap(),
            [AttributeKind::String, AttributeKind::String],
        )
        .unwrap();

    let var_x: Term = Variable::from_str("X").unwrap().into();
    let var_y: Term = Variable::from_str("Y").unwrap().into();
    let var_z: Term = Variable::from_str("Z").unwrap().into();

    graph
        .add_new_rule(
            path.predicate().clone(),
            [var_x.clone(), var_y.clone()],
            [Atom::new(edge.predicate().clone(), [var_x.clone(), var_y.clone()]).into()],
        )
        .unwrap();

    graph
        .add_new_rule(
            path.predicate().clone(),
            [var_x.clone(), var_y.clone()],
            [
                Atom::new(edge.predicate().clone(), [var_x.clone(), var_z.clone()]).into(),
                Atom::new(path.predicate().clone(), [var_z, var_y.clone()]).into(),
            ],
        )
        .unwrap();

    let query = Query::new(path.predicate().clone(), vec![var_x, var_y]);
    graph.add_query(query).unwrap();

    graph.add_relation(edge);

    println!("{}", graph);

    assert_eq_by_line(
        &graph.to_string(),
        r#"@declare edge(string, string).
@declare path(?, ?).

edge(a, b).
edge(c, d).
edge(d, a).
edge(b, c).

path(X, Y) ⟵ edge(X, Y).
path(X, Y) ⟵ edge(X, Z) ⋀ path(Z, Y).

?- path(X, Y).
"#,
    );
}

#[test]
fn test_that_syllogism() {
    // See https://en.wikipedia.org/wiki/Syllogism

    let mut syllogism = Program::default();
    let p_human = Predicate::from_str("human").unwrap();

    let mut human = syllogism
        .make_new_relation(p_human.clone(), [AttributeKind::String])
        .unwrap();
    human.add(["Socrates".into()]);

    syllogism.database_mut().add(human);

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
@declare mortal(string).

human(Socrates).

mortal(X) ⟵ human(X).

?- mortal(Socrates).
"#,
    );
}
