#[cfg(feature = "parser")]
use asdi::parse::parse_str;

#[cfg(feature = "parser")]
#[test]
fn test_wikipedia_example() {
    let program = parse_str(
        r#"parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ parent(Z, Y).

?- ancestor(xerces, X).
"#,
    );
    assert!(program.is_ok());
}

#[cfg(feature = "parser")]
#[test]
fn test_sourceforge_example() {
    let program = parse_str(
        r#"edge(a, b).
edge(b, c).
edge(c, d).
edge(d, a).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

?- path(X, Y).
"#,
    );
    assert!(program.is_ok());
}

#[cfg(feature = "parser")]
#[test]
fn test_that_syllogism() {
    let program = parse_str(
        r#"human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
"#,
    );
    assert!(program.is_ok());
}
