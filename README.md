# Crate asdi

Another Simplistic Datalog Implementation (in Rust).

TBD

# Examples

```datalog
parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y).

?- ancestor(xerces, X).
```

```rust
{
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
}
```