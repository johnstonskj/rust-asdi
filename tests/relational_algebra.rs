use asdi::edb::{Predicate, PredicateRef};
use asdi::idb::query::RelationalOp;
use asdi::idb::{Atom, Comparison, ComparisonOperator, Literal, Rule, Term, Variable};
use std::str::FromStr;

#[test]
fn test_compile_atom_constants() {
    let predicate: PredicateRef = Predicate::from_str("parent").unwrap().into();
    let atom = Atom::new(
        predicate,
        [
            Term::Constant("xerces".into()),
            Term::Constant("brooke".into()),
        ],
    );
    println!(">>> {}", atom);
    let relational = RelationalOp::compile_atom(&atom, true).unwrap();
    println!("<<< {}", relational);
    assert_eq!(
        relational.to_string(),
        "σ[0=xerces, 1=brooke](parent)".to_string()
    );
}

#[test]
#[should_panic]
fn test_compile_atom_constants_fail() {
    let predicate: PredicateRef = Predicate::from_str("parent").unwrap().into();
    let atom = Atom::new(
        predicate,
        [
            Term::Constant("xerces".into()),
            Term::Constant("brooke".into()),
        ],
    );
    // this should panic on `Err` value: NullaryFactsNotAllowed
    RelationalOp::compile_atom(&atom, false).unwrap();
}

#[test]
fn test_compile_atom_variables() {
    let predicate: PredicateRef = Predicate::from_str("parent").unwrap().into();
    let atom = Atom::new(
        predicate,
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let relational = RelationalOp::compile_atom(&atom, true).unwrap();
    assert_eq!(relational.to_string(), "parent".to_string());

    println!(">>> {}", atom);
    let relational = RelationalOp::compile_atom(&atom, false).unwrap();
    println!("<<< {}", relational);
    assert_eq!(relational.to_string(), "parent".to_string());
}

#[test]
fn test_compile_atom_mixed() {
    let predicate: PredicateRef = Predicate::from_str("parent").unwrap().into();
    let atom = Atom::new(
        predicate,
        [
            Term::Constant("xerces".into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let relational = RelationalOp::compile_atom(&atom, true).unwrap();
    assert_eq!(relational.to_string(), "σ[0=xerces](parent)".to_string());

    println!(">>> {}", atom);
    let relational = RelationalOp::compile_atom(&atom, false).unwrap();
    println!("<<< {}", relational);
    assert_eq!(
        relational.to_string(),
        "Π[1](σ[0=xerces](parent))".to_string()
    );
}

#[test]
fn test_compile_rule_one() {
    let head = Atom::new(
        Predicate::from_str("ancestor").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let body = Atom::new(
        Predicate::from_str("parent").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let rule: Rule = Rule::new([head], [Literal::from(body)]);

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    assert_eq!(relational.to_string(), "parent".to_string());
}

#[test]
fn test_compile_rule_two() {
    let head = Atom::new(
        Predicate::from_str("path").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let body_1 = Atom::new(
        Predicate::from_str("edge").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Z").unwrap().into()),
        ],
    );
    let body_2 = Atom::new(
        Predicate::from_str("path").unwrap().into(),
        [
            Term::Variable(Variable::from_str("Z").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let rule: Rule = Rule::new([head], [Literal::from(body_1), Literal::from(body_2)]);

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    println!("{}", relational.to_graphviz_string());
    assert_eq!(
        relational.to_string(),
        "Π[X, Y]((path) ⨝ (edge))".to_string()
    );
}

#[test]
fn test_compile_rule_two_with_rule() {
    let head = Atom::new(
        Predicate::from_str("path").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let body_1 = Atom::new(
        Predicate::from_str("edge").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Z").unwrap().into()),
        ],
    );
    let body_2 = Atom::new(
        Predicate::from_str("path").unwrap().into(),
        [
            Term::Variable(Variable::from_str("Z").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let body_3 = Comparison::new(
        Term::Variable(Variable::from_str("Z").unwrap().into()),
        ComparisonOperator::NotEqual,
        Term::Constant(101.into()),
    )
    .unwrap();
    let rule: Rule = Rule::new(
        [head],
        [
            Literal::from(body_1),
            Literal::from(body_2),
            Literal::from(body_3),
        ],
    );

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    println!("{}", relational.to_graphviz_string());
    assert_eq!(
        relational.to_string(),
        "Π[X, Y]((σ[0!=101](path)) ⨝ (σ[1!=101](edge)))".to_string()
    );
}
