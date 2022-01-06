use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Query, Term, Variable};
use asdi::Program;
use std::rc::Rc;
use std::str::FromStr;

pub mod common;
use common::assert_eq_by_line;

#[test]
fn test_wikipedia_example() {
    // See https://en.wikipedia.org/wiki/Datalog

    let mut ancestors = Program::default();

    let parent_predicate = Rc::new(Predicate::from_str("parent").unwrap());
    {
        let parent = ancestors
            .add_new_extensional_relation(
                parent_predicate.clone(),
                vec![Attribute::string(), Attribute::string()],
            )
            .unwrap();
        parent
            .add_as_fact(["xerces".into(), "brooke".into()])
            .unwrap();
        parent
            .add_as_fact(["brooke".into(), "damocles".into()])
            .unwrap();
    };

    let ancestor_predicate = Rc::new(Predicate::from_str("ancestor").unwrap());

    let var_x: Term = Variable::from_str("X").unwrap().into();
    let var_y: Term = Variable::from_str("Y").unwrap().into();
    let var_z: Term = Variable::from_str("Z").unwrap().into();

    ancestors
        .add_new_pure_rule(
            ancestor_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [Atom::new(parent_predicate.clone(), [var_x.clone(), var_y.clone()]).into()],
        )
        .unwrap();
    ancestors
        .add_new_pure_rule(
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
        r#".assert parent(string, string).

.infer ancestor(string, string).

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

    let edge_predicate = graph.predicates().fetch("edge").unwrap();
    {
        let edge = graph
            .add_new_extensional_relation(
                edge_predicate.clone(),
                vec![Attribute::string(), Attribute::string()],
            )
            .unwrap();
        edge.add_as_fact(["a".into(), "b".into()]).unwrap();
        edge.add_as_fact(["b".into(), "c".into()]).unwrap();
        edge.add_as_fact(["c".into(), "d".into()]).unwrap();
        edge.add_as_fact(["d".into(), "a".into()]).unwrap();
    }

    let path_predicate = graph.predicates().fetch("path").unwrap();

    let var_x: Term = Variable::from_str("X").unwrap().into();
    let var_y: Term = Variable::from_str("Y").unwrap().into();
    let var_z: Term = Variable::from_str("Z").unwrap().into();

    graph
        .add_new_pure_rule(
            path_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [Atom::new(edge_predicate.clone(), [var_x.clone(), var_y.clone()]).into()],
        )
        .unwrap();

    graph
        .add_new_pure_rule(
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
        r#".assert edge(string, string).

.infer path(string, string).

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
    let p_human = syllogism.predicates().fetch("human").unwrap();

    let human = syllogism
        .add_new_extensional_relation(p_human.clone(), vec![Attribute::string()])
        .unwrap();
    human.add_as_fact(["Socrates".into()]).unwrap();

    let var_x: Term = Variable::from_str("X").unwrap().into();

    syllogism
        .add_new_pure_rule(
            syllogism.predicates().fetch("mortal").unwrap(),
            [var_x.clone()],
            [Atom::new(p_human, [var_x]).into()],
        )
        .unwrap();

    syllogism
        .add_new_query(
            syllogism.predicates().fetch("mortal").unwrap(),
            ["Socrates".into()],
        )
        .unwrap();

    println!("{}", syllogism);

    assert_eq_by_line(
        &syllogism.to_string(),
        r#".assert human(string).

.infer mortal(string).

human("Socrates").

mortal(X) ⟵ human(X).

?- mortal("Socrates").
"#,
    );
}
