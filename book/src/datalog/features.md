# Language Features

By default, the language supported by ASDI is sometimes termed _Pure Datalog_. This language
allows for positive literals only, and allow for recursion. It does support additional features
with feature sets applied to programs. Without a feature specified certain types will fail
well-formedness rules, the parser will report errors, and some tools will not execute. However,
the enabling of features is relatively simple in both the text representation using the `.feature`
pragma, and using `FeatureSet`s in the API.

## Negation

The feature `negation` enables the negation of individual literals in the body of a rule. This
language is often described as $\text{\small{Datalog}}^{\lnot}$.

```datalog
.features(negation).
alive(X) :- person(X) AND NOT dead(X).
alive(X) :- person(X) ∧ ￢dead(X).
```

The text representation allows for `"!"`, `"￢"`, and `"NOT"` to be used to denote negation.

## Constraints

The feature `constraints` enables the specification of rules that have no head, which in turn
specifies a rule that _may never_ be true. This language is described herein as
$\text{\small{Datalog}}^{\bot}$.

```datalog
.features(constraints).
⊥ :- dead(X) AND alive(X).
:- dead(X) AND alive(X).
```

The text allows for either an entirely missing head, or the value `"⊥"` as the head to denote
a constraint. The latter signifies that the rule implies falsity (or absurdity).

## Comparisons

The feature `comparisons` enables the inclusion of literal terms that use standard comparison
operators. This language is described herein as $\text{\small{Datalog}}^{\theta}$.

```datalog
.features(comparisons).
old(X) :- age(X, Y) ∧ Y > 75.
```

The text representation supports the operators equality (`"="`), inequality (`"!="`, `"/="`,
or `"≠"`), less than (`"<"`), less than or equal-to (`"<="`, or `"≤"`), greater than (`">"`), and
greater than or equal-to (`">="`, or `"≥"`).

## Disjunction

The feature `disjunction` enables the negation of individual literals in the body of a rule. This
language is often described as $\text{\small{Datalog}}^{\lor}$.

```datalog
.features(disjunction).
mother(X, Y) OR father(X, Y) :- parent(X, Y).
mother(X, Y) ∨ father(X, Y) :- parent(X, Y).
```

The text representation allows for `";"`, "|"`, `"∨"`, and `"OR"` to be used to denote disjunction.


## Example

The following demonstrates the text representation support for enabling features.

```datalog
.features(negation, comparisons, disjunction).
```

Similarly, the following API example shows how to create a feature set that may be added to a
program during creation.

```rust,no_run
use asdi::features::{
    FEATURE_COMPARISONS, FEATURE_DISJUNCTION, FEATURE_NEGATION, FeatureSet
};

let features = FeatureSet::from(vec![FEATURE_NEGATION, FEATURE_DISJUNCTION]);

assert!(features.supports(&FEATURE_NEGATION));
assert!(!features.supports(&FEATURE_COMPARISONS));

assert_eq!(features.to_string(), ".feature(negation, disjunction).");
```
