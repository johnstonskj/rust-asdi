use asdi::{Atom, Fact, Program, Query, Rule, Term};
use pretty_assertions::assert_eq;

#[test]
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

    let head = Atom::new_with_parameters(ancestor.clone(), vec![var_x.clone(), var_y.clone()]);

    ancestors.push(Fact::new_with_parameters(
        parent.clone(),
        vec![xerces.clone(), brooke.clone()],
    ));
    ancestors.push(Fact::new_with_parameters(
        parent.clone(),
        vec![brooke, damocles],
    ));

    ancestors.push(Rule::new(
        head.clone(),
        vec![Atom::new_with_parameters(
            parent.clone(),
            vec![var_x.clone(), var_y.clone()],
        )],
    ));
    ancestors.push(Rule::new(
        head,
        vec![
            Atom::new_with_parameters(parent.clone(), vec![var_x.clone(), var_z.clone()]),
            Atom::new_with_parameters(parent, vec![var_z, var_y]),
        ],
    ));

    let query = Atom::new_with_parameters(ancestor, vec![xerces.into(), var_x]);

    ancestors.push(Query::from(query));

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

    graph.push(Fact::new_with_parameters(
        edge.clone(),
        vec![edge_a.clone(), edge_b.clone()],
    ));

    graph.push(Fact::new_with_parameters(
        edge.clone(),
        vec![edge_b, edge_c.clone()],
    ));

    graph.push(Fact::new_with_parameters(
        edge.clone(),
        vec![edge_c, edge_d.clone()],
    ));

    graph.push(Fact::new_with_parameters(
        edge.clone(),
        vec![edge_d, edge_a],
    ));

    let head = Atom::new_with_parameters(path.clone(), vec![var_x.clone(), var_y.clone()]);

    graph.push(Rule::new(
        head.clone(),
        vec![Atom::new_with_parameters(
            edge.clone(),
            vec![var_x.clone(), var_y.clone()],
        )],
    ));
    graph.push(Rule::new(
        head,
        vec![
            Atom::new_with_parameters(edge, vec![var_x.clone(), var_z.clone()]),
            Atom::new_with_parameters(path.clone(), vec![var_z, var_y.clone()]),
        ],
    ));

    let query = Atom::new_with_parameters(path, vec![var_x, var_y]);

    graph.push(Query::from(query));

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
fn test_that_syllogism() {
    // See https://en.wikipedia.org/wiki/Syllogism

    let mut syllogism = Program::default();

    let mortal = syllogism.make_predicate_relation("mortal").unwrap();
    let human = syllogism.make_predicate_relation("human").unwrap();

    let socrates = syllogism.make_constant_string("Socrates").unwrap();

    let var_x: Term = syllogism.make_term_variable("X").unwrap();

    syllogism.push(Fact::new_with_parameters(
        human.clone(),
        vec![socrates.clone()],
    ));

    syllogism.push(Rule::new(
        Atom::new_with_parameters(mortal.clone(), vec![var_x.clone()]),
        vec![Atom::new_with_parameters(human, vec![var_x])],
    ));

    let query = Atom::new_with_parameters(mortal, vec![socrates.into()]);

    syllogism.push(Query::from(query));

    assert_eq!(
        syllogism.to_string(),
        r#"human("Socrates").
mortal(X) ⟵ human(X).
?- mortal("Socrates").
"#
    );
}
