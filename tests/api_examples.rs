use asdi::{Atom, Fact, Program, Query, Rule, Term};
use pretty_assertions::assert_eq;

#[test]
#[ignore]
fn test_wikipedia_example() {
    // See https://en.wikipedia.org/wiki/Datalog

    let mut ancestors = Program::default();

    let parent = ancestors.make_predicate_relation("parent").unwrap();
    let ancestor = ancestors.make_predicate_relation("ancestor").unwrap();

    let xerces = ancestors.make_constant_identifier("xerces").unwrap();
    let brooke = ancestors.make_constant_identifier("brooke").unwrap();
    let damocles = ancestors.make_constant_identifier("damocles").unwrap();

    let var_x: Term = ancestors.make_term_variable("X").unwrap();
    let var_y: Term = ancestors.make_term_variable("Y").unwrap();
    let var_z: Term = ancestors.make_term_variable("Z").unwrap();

    let head = Atom::new(ancestor.clone(), vec![var_x.clone(), var_y.clone()]);

    ancestors
        .push(Fact::new_with_arguments(
            parent.clone(),
            vec![xerces.clone(), brooke.clone()],
        ))
        .unwrap();
    ancestors
        .push(Fact::new_with_arguments(
            parent.clone(),
            vec![brooke, damocles],
        ))
        .unwrap();

    ancestors
        .push(Rule::new_with_body(
            head.clone(),
            vec![Atom::new(parent.clone(), vec![var_x.clone(), var_y.clone()]).into()],
        ))
        .unwrap();
    ancestors
        .push(Rule::new_with_body(
            head,
            vec![
                Atom::new(parent.clone(), vec![var_x.clone(), var_z.clone()]).into(),
                Atom::new(parent, vec![var_z, var_y]).into(),
            ],
        ))
        .unwrap();

    let query = Atom::new(ancestor, vec![xerces.into(), var_x]);

    ancestors.push(Query::from(query)).unwrap();

    println!("{:#?}", ancestors);

    assert_eq!(
        ancestors.to_string(),
        r#"parent(xerces, brooke).
parent(brooke, damocles).
ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y).
?- ancestor(xerces, X).
"#
    )
}

#[test]
#[ignore]
fn test_sourceforge_example() {
    // See http://datalog.sourceforge.net/datalog.html

    let mut graph = Program::default();

    let edge = graph.make_predicate_relation("edge").unwrap();
    let path = graph.make_predicate_relation("path").unwrap();

    let edge_a = graph.make_constant_identifier("a").unwrap();
    let edge_b = graph.make_constant_identifier("b").unwrap();
    let edge_c = graph.make_constant_identifier("c").unwrap();
    let edge_d = graph.make_constant_identifier("d").unwrap();

    let var_x: Term = graph.make_term_variable("X").unwrap();
    let var_y: Term = graph.make_term_variable("Y").unwrap();
    let var_z: Term = graph.make_term_variable("Z").unwrap();

    graph
        .push(Fact::new_with_arguments(
            edge.clone(),
            vec![edge_a.clone(), edge_b.clone()],
        ))
        .unwrap();

    graph
        .push(Fact::new_with_arguments(
            edge.clone(),
            vec![edge_b, edge_c.clone()],
        ))
        .unwrap();

    graph
        .push(Fact::new_with_arguments(
            edge.clone(),
            vec![edge_c, edge_d.clone()],
        ))
        .unwrap();

    graph
        .push(Fact::new_with_arguments(edge.clone(), vec![edge_d, edge_a]))
        .unwrap();

    let head = Atom::new(path.clone(), vec![var_x.clone(), var_y.clone()]);

    graph
        .push(Rule::new_with_body(
            head.clone(),
            vec![Atom::new(edge.clone(), vec![var_x.clone(), var_y.clone()]).into()],
        ))
        .unwrap();
    graph
        .push(Rule::new_with_body(
            head,
            vec![
                Atom::new(edge, vec![var_x.clone(), var_z.clone()]).into(),
                Atom::new(path.clone(), vec![var_z, var_y.clone()]).into(),
            ],
        ))
        .unwrap();

    let query = Atom::new(path, vec![var_x, var_y]);

    graph.push(Query::from(query)).unwrap();

    assert_eq!(
        graph.to_string(),
        r#"edge(a, b).
edge(b, c).
edge(c, d).
edge(d, a).
path(X, Y) ⟵ edge(X, Y).
path(X, Y) ⟵ edge(X, Z) ⋀ path(Z, Y).
?- path(X, Y).
"#
    );
}

#[test]
#[ignore]
fn test_that_syllogism() {
    // See https://en.wikipedia.org/wiki/Syllogism

    let mut syllogism = Program::default();

    let mortal = syllogism.make_predicate_relation("mortal").unwrap();
    let human = syllogism.make_predicate_relation("human").unwrap();

    let socrates = syllogism.make_constant_string("Socrates").unwrap();

    let var_x: Term = syllogism.make_term_variable("X").unwrap();

    syllogism
        .push(Fact::new_with_arguments(
            human.clone(),
            vec![socrates.clone()],
        ))
        .unwrap();

    syllogism
        .push(Rule::new_with_body(
            Atom::new(mortal.clone(), vec![var_x.clone()]),
            vec![Atom::new(human, vec![var_x]).into()],
        ))
        .unwrap();

    let query = Atom::new(mortal, vec![socrates.into()]);

    syllogism.push(Query::from(query)).unwrap();

    assert_eq!(
        syllogism.to_string(),
        r#"human("Socrates").
mortal(X) ⟵ human(X).
?- mortal("Socrates").
"#
    );
}
