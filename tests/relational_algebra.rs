use asdi::edb::{Predicate, PredicateRef};
use asdi::features::{FeatureSet, FEATURE_NEGATION};
use asdi::idb::query::relational::{program_to_graphviz, RelationalOp};
use asdi::idb::{Atom, Comparison, ComparisonOperator, Literal, Rule, Term, Variable};
use asdi::parse::parse_str;
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
    rule.safety_check(&FeatureSet::from(FEATURE_NEGATION))
        .unwrap();

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    assert_eq!(relational.to_string(), "ancestor ≔ parent".to_string());
}

#[test]
fn test_compile_rule_one_criteria() {
    let head = Atom::new(
        Predicate::from_str("unused").unwrap().into(),
        [Term::Variable(Variable::from_str("X").unwrap().into())],
    );
    let body_atom = Atom::new(
        Predicate::from_str("done_something").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Variable(Variable::from_str("Y").unwrap().into()),
        ],
    );
    let body_comp = Comparison::ne(
        Term::Variable(Variable::from_str("Y").unwrap().into()),
        Term::Constant(0.into()),
    )
    .unwrap();
    let rule: Rule = Rule::new([head], [Literal::from(body_atom), Literal::from(body_comp)]);
    rule.safety_check(&FeatureSet::from(FEATURE_NEGATION))
        .unwrap();

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    assert_eq!(
        relational.to_string(),
        "unused ≔ Π[X](σ[1!=0](done_something))".to_string()
    );
}

#[test]
#[ignore]
fn test_compile_rule_negated_literal() {
    let head = Atom::new(
        Predicate::from_str("living").unwrap().into(),
        [Term::Variable(Variable::from_str("X").unwrap().into())],
    );
    let body_1 = Atom::new(
        Predicate::from_str("person").unwrap().into(),
        [Term::Variable(Variable::from_str("X").unwrap().into())],
    );
    let body_2 = Atom::new(
        Predicate::from_str("died_on").unwrap().into(),
        [
            Term::Variable(Variable::from_str("X").unwrap().into()),
            Term::Anonymous,
        ],
    );
    let rule: Rule = Rule::new(
        [head],
        [
            Literal::relational(body_1),
            Literal::negative_relational(body_2),
        ],
    );
    rule.safety_check(&FeatureSet::from(FEATURE_NEGATION))
        .unwrap();

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    assert_eq!(
        relational.to_string(),
        "living ≔ (person) ∖ ((died_on) ⨝ (person))".to_string()
    );
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
    rule.safety_check(&FeatureSet::from(FEATURE_NEGATION))
        .unwrap();

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    println!("{}", relational.to_graphviz_string().unwrap());
    assert_eq!(
        relational.to_string(),
        "path ≔ Π[X, Y]((path) ⨝ (edge))".to_string()
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
    rule.safety_check(&FeatureSet::from(FEATURE_NEGATION))
        .unwrap();

    println!(">>> {}", rule);
    let relational = RelationalOp::compile_rule(&rule).unwrap();
    println!("<<< {}", relational);
    println!("{}", relational.to_graphviz_string().unwrap());
    assert_eq!(
        relational.to_string(),
        "path ≔ Π[X, Y]((σ[0!=101](path)) ⨝ (σ[1!=101](edge)))".to_string()
    );
}

#[test]
fn test_large_program() {
    const PROGRAM_SOURCE: &str = r#".assert triple(subject: string, predicate: string, object: string).

% Section 2.1: Resoure
triple(rdfs:Resource, rdf:type, rdfs:Class).

% Section 2.2: Class
triple(rdfs:Class, rdf:type, rdfs:Class).

% Section 2.3: Literal
triple(rdfs:Literal, rdf:type, rdfs:Class).
triple(rdfs:Literal, rdfs:subClassOf, rdfs:Resource).

% Section 2.4: Datatype
triple(rdfs:Datatype, rdf:type, rdfs:Class).
triple(rdfs:Datatype, rdfs:subClassOf, rdfs:Class).

% Section 2.5: langString
triple(rdf:langString, rdf:type, rdfs:Datatype).
triple(rdf:langString, rdfs:subClassOf, rdfs:Literal).

% Section 2.6: HTML
triple(rdf:HTML, rdf:type, rdfs:Datatype).
triple(rdf:HTML, rdfs:subClassOf, rdfs:Literal).

% Section 2.7: XMLLiteral
triple(rdf:XMLLiteral, rdf:type, rdfs:Datatype).
triple(rdf:XMLLiteral, rdfs:subClassOf, rdfs:Literal).

% Section 2.8: Property
triple(rdf:Property, rdf:type, rdfs:Class).

% Section 3.1: range
triple(rdfs:range, rdfs:type, rdfs:Property).
triple(rdfs:range, rdfs:domain, rdfs:Property).
triple(rdfs:range, rdfs:range, rdfs:Class).

% Section 3.2: domain
triple(rdfs:domain, rdf:type, rdfs:Property).
triple(rdfs:domain, rdfs:domain, rdfs:Property).
triple(rdfs:domain, rdfs:range, rdfs:Class).

% Section 3.3: type
triple(rdf:type, rdf:type, rdfs:Property).
triple(rdfs:type, rdfs:domain, rdfs:Resource).
triple(rdfs:type, rdfs:range, rdfs:Class).

% Section 3.4: subClassOf
triple(rdfs:subClassOf, rdf:type, rdfs:Property).
triple(rdfs:subClassOf, rdfs:domain, rdfs:Class).
triple(rdfs:subClassOf, rdfs:range, rdfs:Class).

% Section 3.5: subPropertyOf
triple(rdfs:subPropertyOf, rdf:type, rdfs:Property).
triple(rdfs:subPropertyOf, rdfs:domain, rdfs:Property).
triple(rdfs:subPropertyOf, rdfs:range, rdfs:Property).

% Section 3.6: label
triple(rdfs:label, rdf:type, rdfs:Property).
triple(rdfs:label, rdfs:domain, rdf:Resource).
triple(rdfs:label, rdfs:range, rdfs:Literal).

% Section 3.7: comment
triple(rdfs:comment, rdf:type, rdfs:Property).
triple(rdfs:comment, rdfs:domain, rdf:Resource).
triple(rdfs:comment, rdfs:range, rdfs:Literal).


% -------------------------------------------------------------------------------

triple(ex:Car, rdfs:subClassOf, ex:Vehicle).

triple(ex:name, rdfs:subPropertyOf, rdfs:label).
triple(ex:name, rdfs:domain, ex:Car).
triple(ex:name, rdfs:range, xsd:string).

triple(ex:my_car, ex:name, "the clown car").

% -------------------------------------------------------------------------------

% Section: 2.1: Resource -- basically uninteresing as all things are resources.
% resource(R) :- triple(R, _, _).
% resource(R) :- triple(_, R, _).
% resource(R) :- triple(_, _, R).

% Section 2.2: Class
class(C) :- triple(_, rdf:type, C).

% Section 2.4: Datatype
subClass(R, rdfs:Literal) :- instanceOf(R, rdfs:Datatype).

% Section 2.8: Property
property(P) :- triple(_, P, _).
property(P) :- triple(P, rdf:type, rdfs:Property).

% Section 3.1: range
range(P, C) :- triple(P, rdfs:range, C).

% Section 3.2: domain
domain(P, C) :- triple(P, rdfs:domain, C).

% Section 3.3: rdf:type
instanceOf(R, C) :- triple(R, rdf:type, C).
instanceOf(R, C) :- triple(R, P, _), triple(P, rdfs:domain, C).
instanceOf(R, C) :- triple(_, P, R), triple(P, rdfs:range, C).

% Section 3.4: subClassOf
subClass(C, P) :- triple(C, rdfs:subClassOf, P).
class(C) :- subClass(C, _).
class(C) :- subClass(_, C).
% ----- NEGATION: subClass(C, rdfs:Class) :- class(C) AND NOT class(rdfs:Class).
instanceOf(C, C2) :- instanceOf(C, C1), subClass(C1, C2).

% Section 3.5: subPropertyOf
subProperty(C, P) :- triple(C, rdfs:subPropertyOf, P).
property(P) :- subProperty(P, _).
property(P) :- subProperty(_, P).
% ----- NEGATION: subProperty(P, rdfs:Property) :- property(P) AND NOT property(rdfs:Property).
instanceOf(P, C2) :- instanceOf(P, C1), subProperty(C1, C2).
"#;
    let program = parse_str(PROGRAM_SOURCE).unwrap().into_parsed();

    println!("{}", program_to_graphviz(&program));
}
