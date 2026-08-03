//! Coverage for the `RelationOps` arms on `View` that were previously
//! `todo!()`/`unimplemented!()` — `union`, `intersection`,
//! `cartesian_product`. See IMPROVEMENTS.md §9.1.
//!
//! The trait is `pub`, so any of these arms panicking is a runtime
//! foot-gun for downstream library users.

use asdi::edb::{Attribute, AttributeKind, Constant};
use asdi::idb::query::{Row, View};
use asdi::idb::query::relational::RelationOps;
use asdi::idb::Variable;
use asdi::Collection;
use std::rc::Rc;
use std::str::FromStr;

fn s(v: &str) -> Constant {
    v.to_string().into()
}

fn labeled_attr(name: &str) -> Attribute<Variable> {
    let var = Rc::new(Variable::from_str(name).unwrap());
    Attribute::new(var, AttributeKind::String)
}

/// Union of two views with identical schemas is the set-union of
/// their rows — duplicates collapse.
#[test]
fn union_dedups_and_keeps_schema() {
    let schema = vec![labeled_attr("X")];
    let a = View::new_with_facts(
        schema.clone(),
        vec![Row::from(s("alice")), Row::from(s("bob"))],
    );
    let b = View::new_with_facts(
        schema.clone(),
        vec![Row::from(s("bob")), Row::from(s("carol"))],
    );

    let u = a.union(b).expect("union should succeed");
    assert_eq!(u.len(), 3, "expected {{alice, bob, carol}} (dedup), got {}", u.len());
}

#[test]
fn union_mismatched_schemas_errors_not_panics() {
    let a = View::new_with_facts(vec![labeled_attr("X")], vec![Row::from(s("alice"))]);
    let b = View::new_with_facts(vec![labeled_attr("Y")], vec![Row::from(s("alice"))]);
    let result = a.union(b);
    assert!(
        result.is_err(),
        "union over mismatched schemas should be Err, not panic"
    );
}

#[test]
fn intersection_keeps_only_shared_rows() {
    let schema = vec![labeled_attr("X")];
    let a = View::new_with_facts(
        schema.clone(),
        vec![Row::from(s("alice")), Row::from(s("bob"))],
    );
    let b = View::new_with_facts(
        schema,
        vec![Row::from(s("bob")), Row::from(s("carol"))],
    );

    let i = a.intersection(b).expect("intersection should succeed");
    assert_eq!(i.len(), 1, "expected {{bob}}, got {} rows", i.len());
    assert!(i.contains(&Row::from(s("bob"))));
}

#[test]
fn intersection_mismatched_schemas_errors_not_panics() {
    let a = View::new_with_facts(vec![labeled_attr("X")], vec![Row::from(s("alice"))]);
    let b = View::new_with_facts(vec![labeled_attr("Y")], vec![Row::from(s("alice"))]);
    let result = a.intersection(b);
    assert!(result.is_err());
}

#[test]
fn cartesian_product_combines_rows() {
    let a = View::new_with_facts(
        vec![labeled_attr("X")],
        vec![Row::from(s("a1")), Row::from(s("a2"))],
    );
    let b = View::new_with_facts(
        vec![labeled_attr("Y")],
        vec![Row::from(s("b1")), Row::from(s("b2")), Row::from(s("b3"))],
    );

    let p = a.cartesian_product(b).expect("product should succeed");
    assert_eq!(p.len(), 2 * 3, "|A| × |B| = 6 rows");
    assert_eq!(p.schema().len(), 2, "result schema has both X and Y");
}

/// Schemas sharing a label aren't a valid cartesian-product input
/// (they'd produce duplicate-label result schema). Must error, not
/// panic.
#[test]
fn cartesian_product_overlapping_schemas_errors() {
    let a = View::new_with_facts(vec![labeled_attr("X")], vec![Row::from(s("a"))]);
    let b = View::new_with_facts(vec![labeled_attr("X")], vec![Row::from(s("b"))]);
    let result = a.cartesian_product(b);
    assert!(
        result.is_err(),
        "shared schema labels should yield Err, not panic"
    );
}

/// The historic `difference` arm worked but panicked on schema
/// mismatch. After §9.1 it should error cleanly like its siblings.
#[test]
fn difference_mismatched_schemas_errors_not_panics() {
    let a = View::new_with_facts(vec![labeled_attr("X")], vec![Row::from(s("a"))]);
    let b = View::new_with_facts(vec![labeled_attr("Y")], vec![Row::from(s("a"))]);
    let result = a.difference(b);
    assert!(result.is_err());
}
