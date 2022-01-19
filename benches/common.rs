use asdi::idb::eval::Evaluator;
use asdi::parse::parse_str;
use asdi::Program;

pub fn socrates_program() -> Program {
    const PROGRAM_SOURCE: &str = r#"human("Socrates").
human("Plato").

mortal(X) <- human(X).

?- mortal("Socrates").
"#;
    parse_str(PROGRAM_SOURCE).unwrap().into_parsed()
}

pub fn eval_socrates<T: Evaluator>(program: &Program, evaluator: T) {
    let _ = evaluator.inference(program).unwrap();
}

// ------------------------------------------------------------------------------------------------

pub fn ancestors_program() -> Program {
    const PROGRAM_SOURCE: &str = r#"parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ parent(Z, Y).

?- ancestor(xerces, X).
"#;
    parse_str(PROGRAM_SOURCE).unwrap().into_parsed()
}

pub fn eval_ancestors<T: Evaluator>(program: &Program, evaluator: T) {
    let _ = evaluator.inference(program).unwrap();
}

// ------------------------------------------------------------------------------------------------

pub fn paths_program() -> Program {
    const PROGRAM_SOURCE: &str = r#".assert edge(string, string).

edge(a, b).
edge(c, d).
edge(d, a).
edge(b, c).

path(X, Y) ⟵ edge(X, Y).
path(X, Y) ⟵ edge(X, Z) ∧ path(Z, Y).

?- path(X, Y).
"#;
    parse_str(PROGRAM_SOURCE).unwrap().into_parsed()
}

pub fn eval_paths<T: Evaluator>(program: &Program, evaluator: T) {
    let _ = evaluator.inference(program).unwrap();
}

pub fn rdf_schema_program() -> Program {
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
    parse_str(PROGRAM_SOURCE).unwrap().into_parsed()
}

pub fn eval_rdf_schema<T: Evaluator>(program: &Program, evaluator: T) {
    let _ = evaluator.inference(program).unwrap();
}
