use asdi::edb::Predicate;
use asdi::idb::eval::{Evaluator, NaiveEvaluator};
use asdi::idb::query::{Query, Queryable};
use asdi::parse::parse_str;
use asdi::{Collection, ProgramCore};
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

    let p_human = program.predicates().fetch("human").unwrap();
    let p_mortal = program.predicates().fetch("mortal").unwrap();

    assert_eq!(
        program
            .extensional()
            .get(&Predicate::from_str("human").unwrap())
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        program
            .intensional()
            .get(&Predicate::from_str("mortal").unwrap())
            .unwrap()
            .len(),
        0
    );

    print!("{}", program);

    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let new_intensional = evaluator.inference(&program).unwrap();

    assert_eq!(program.extensional().get(&p_human).unwrap().len(), 2);
    assert_eq!(new_intensional.get(&p_mortal).unwrap().len(), 2);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().iter().next().unwrap();

    let results = new_intensional.query(query).unwrap().unwrap();
    println!("{}", results);
    assert_eq!(results.schema().len(), 1);
    assert_eq!(results.len(), 1);

    print!("{} ==>\n{}", query, results);
    println!("-------------------------------------------------------------------------------");

    let query = Query::new(p_mortal, [program.variables().fetch("X").unwrap().into()]);

    let results = new_intensional.query(&query).unwrap().unwrap();
    assert_eq!(results.schema().len(), 1);
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

    let p_parent = program.predicates().fetch("parent").unwrap();
    let p_ancestor = program.predicates().fetch("ancestor").unwrap();

    assert_eq!(program.extensional().get(&p_parent).unwrap().len(), 2);
    assert_eq!(program.intensional().get(&p_ancestor).unwrap().len(), 0);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();
    let results = evaluator.inference(&program);
    program
        .intensional_mut()
        .merge_from(results.unwrap())
        .unwrap();

    assert_eq!(program.extensional().get(&p_parent).unwrap().len(), 2);
    assert_eq!(program.intensional().get(&p_ancestor).unwrap().len(), 3);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().iter().next().unwrap();

    let results = program.intensional().query(query).unwrap().unwrap();
    println!("{}", results);
    assert_eq!(results.schema().len(), 2);
    assert_eq!(results.len(), 2);

    print!("{} ==>\n{}", query, results);
}

#[test]
fn test_sourceforge_example() {
    const PROGRAM_SOURCE: &str = r#".assert edge(string, string).

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

    let p_edge = program.predicates().fetch("edge").unwrap();
    let p_path = program.predicates().fetch("path").unwrap();

    assert_eq!(program.extensional().get(&p_edge).unwrap().len(), 4);
    assert_eq!(program.intensional().get(&p_path).unwrap().len(), 0);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program).unwrap();

    assert_eq!(program.extensional().get(&p_edge).unwrap().len(), 4);
    assert_eq!(results.get(&p_path).unwrap().len(), 16);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let query = program.queries().iter().next().unwrap();

    let results = results.query(query).unwrap().unwrap();
    assert_eq!(results.schema().len(), 2);
    assert_eq!(results.len(), 16);

    print!("{} ==>\n{}", query, results);
}

#[test]
fn test_rdfs() {
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
    let mut program = parse_str(PROGRAM_SOURCE).unwrap().into_parsed();

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program);
    assert!(results.is_ok());
    let new_relations = results.unwrap();

    program.intensional_mut().merge_from(new_relations).unwrap();

    print!("{}", program);

    print!("{}", PROGRAM_SOURCE);
    println!("-------------------------------------------------------------------------------");

    let program = parse_str(PROGRAM_SOURCE).unwrap().into_parsed();

    let p_triple = program.predicates().fetch("triple").unwrap();
    let p_instance_of = program.predicates().fetch("instanceOf").unwrap();

    assert_eq!(program.extensional().get(&p_triple).unwrap().len(), 39);
    assert_eq!(program.intensional().get(&p_instance_of).unwrap().len(), 0);

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();
    let results = evaluator.inference(&program).unwrap();

    assert_eq!(program.extensional().get(&p_triple).unwrap().len(), 39);
    assert_eq!(results.get(&p_instance_of).unwrap().len(), 29);

    print!("{}", program);
}
