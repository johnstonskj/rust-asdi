use asdi::idb::{Evaluator, NaiveEvaluator};
use asdi::parse::parse_str;

#[test]
fn test_rdfs() {
    const PROGRAM_SOURCE: &str = r#"@assert triple(subject: string, predicate: string, object: string).

@infer class(resource: string).

# Section 2.1: Resoure
triple(rdfs:Resource, "rdf:type", "rdfs:Class").

# Section 2.2: Class
triple("rdfs:Class", "rdf:type", "rdfs:Class").

# Section 2.3: Literal
triple("rdfs:Literal", "rdf:type", "rdfs:Class").
triple("rdfs:Literal", "rdf:subClassOf", "rdfs:Resource").

# Section 2.8: Property
triple("rdf:Property", "rdf:type", "rdfs:Class").

# Section 3.1: range
triple("rdfs:range", "rdfs:type", "rdfs:Property").

# Section 3.2: domain
triple("rdf:domain", "rdf:type", "rdfs:Property").

# Section 3.3: type
triple("rdf:type", "rdf:type", "rdf:Property").

# Section 3.4: subClassOf
triple("rdfs:subClassOf", "rdf:type", "rdf:Property").

# Section 3.4: subPropertyOf
triple("rdfs:subPropertyOf", "rdf:type", "rdf:Property").

# -------------------------------------------------------------------------------

triple("foo", "rdf:type", "Foo").
triple("foo", "rdfs:range", "String").
triple("foo", "rdfs:domain", "Thing").

# -------------------------------------------------------------------------------

instanceOf(R, C) :- triple(R, "rdf:type", C).
class(C) :- triple(_, "rdf:type", C).

# Section 3.1: range
instanceOf(P, "rdfs:Property") :- triple(P, "rdfs:range", _).
instanceOf(C, "rdfs:Class") :- triple(_, "rdfs:range", C).

# Section 3.2: domain
instanceOf(P, "rdfs:Property") :- triple(P, "rdfs:domain", _).
instanceOf(C, "rdfs:Class") :- triple(_, "rdfs:domain", C).

# Section 3.4: subClassOf
instanceOf(C, "rdfs:Class") :- triple(C, "rdfs:subClassOf", _).
instanceOf(C, "rdfs:Class") :- triple(_, "rdfs:subClassOf", C).

# Section 3.4: subPropertyOf
instanceOf(P, "rdfs:Class") :- triple(P, "rdfs:subPropertyOf", _).
instanceOf(P, "rdfs:Class") :- triple(_, "rdfs:subPropertyOf", P).
"#;

    print!("{}", PROGRAM_SOURCE);
    println!("-------------------------------------------------------------------------------");

    let mut program = parse_str(PROGRAM_SOURCE).unwrap().into_parsed();

    print!("{}", program);
    println!("-------------------------------------------------------------------------------");

    let evaluator = NaiveEvaluator::default();

    let results = evaluator.inference(&program);

    program.extensional_mut().merge(results.unwrap()).unwrap();

    print!("{}", program);
}
