/*!
This module provides the `Feature` and `FeatureSet` types that allow programs to support specific
extensions to Datalog.

By default the language supported by this library is sometimes termed _Pure Datalog_. This language
allows for positive literals only, and allow for recursion. It does support additional features
with feature sets applied to programs. Without a feature specified certain types will fail
well-formedness rules, the parser will report errors, and some tools will not execute. However,
the enabling of features is relatively simple in both the text representation using the `@feature`
pragma, and using `FeatureSet`s in the API.

# Example

The following demonstrates the text representation support for enabling features.

```datalog
@features(negation, comparisons, disjunction).
```

Similarly, the following API example shows how to create a feature set that may be added to a
program during creation.

```rust
use asdi::features::{
    FEATURE_COMPARISONS, FEATURE_DISJUNCTION, FEATURE_NEGATION, FeatureSet
};

let features = FeatureSet::from(vec![FEATURE_NEGATION, FEATURE_DISJUNCTION]);

assert!(features.supports(&FEATURE_NEGATION));
assert!(!features.supports(&FEATURE_COMPARISONS));

assert_eq!(features.to_string(), "@feature(negation, disjunction).");
```

# Supported Features

The following describe the set of currently supported features.

## Negation

The feature `negation` enables the negation of individual literals in the body of a rule. This
language is often described as $\text{\small{Datalog}}^{\lnot}$.

```datalog
@features(negation).
alive(X) :- person(X) AND NOT dead(X).
alive(X) :- person(X) ∧ ￢dead(X).
```

The text representation allows for `"!"`, `"￢"`, and `"NOT"` to be used to denote negation.

## Constraints

The feature `constraints` enables the specification of rules that have no head, which in turn
specifies a rule that _may never_ be true. This language is described herein as
$\text{\small{Datalog}}^{\bot}$.

```datalog
@features(constraints).
⊥ :- dead(X) AND alive(X).
:- dead(X) AND alive(X).
```

The text allows for either an entirely missing head, or the value `"⊥"` as the head to denote
a constraint. The latter signifies that the rule implies falsity (or absurdity).

## Comparisons

The feature `comparisons` enables the inclusion of literal terms that use standard comparison
operators. This language is described herein as $\text{\small{Datalog}}^{=}$.

```datalog
@features(comparisons).
old(X) :- age(X, Y) ∧ Y > 75.
```

The text representation supports the operators equality (`"="`), inequality (`"!="`, `"/="`,
or `"≠"`), less than (`"<"`), less than or equal-to (`"<="`, or `"≤"`), greater than (`">"`), and
greater than or equal-to (`">="`, or `"≥"`).

## Disjunction

The feature `disjunction` enables the negation of individual literals in the body of a rule. This
language is often described as $\text{\small{Datalog}}^{\lor}$.

```datalog
@features(disjunction).
mother(X, Y) OR father(X, Y) :- parent(X, Y).
mother(X, Y) ∨ father(X, Y) :- parent(X, Y).
```

The text representation allows for `";"`, "|"`, `"∨"`, and `"OR"` to be used to denote disjunction.

*/

use crate::syntax::{
    CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, DEFAULT_LANGUAGE_NAME,
    DISJUNCTION_UNICODE_SYMBOL, FALSE_UNICODE_SYMBOL, NOT_UNICODE_SYMBOL, OPERATOR_ASCII_EQUAL,
    PRAGMA_FEATURE_COMPARISONS, PRAGMA_FEATURE_CONSTRAINTS, PRAGMA_FEATURE_DISJUNCTION,
    PRAGMA_FEATURE_NEGATION, RESERVED_PRAGMA_FEATURE, RESERVED_PREFIX,
};
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// This type represents a set of features that have been, or will be, enabled for a particular
/// program.
///
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FeatureSet(u8);

///
/// This type describes a particular feature that may be enabled for any program.
///
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Feature {
    label: &'static str,
    symbol: &'static str,
    mask: u8,
}

pub const FEATURE_NEGATION: Feature = Feature {
    label: PRAGMA_FEATURE_NEGATION,
    symbol: NOT_UNICODE_SYMBOL,
    mask: 1,
};

pub const FEATURE_COMPARISONS: Feature = Feature {
    label: PRAGMA_FEATURE_COMPARISONS,
    symbol: OPERATOR_ASCII_EQUAL,
    mask: 2,
};

pub const FEATURE_DISJUNCTION: Feature = Feature {
    label: PRAGMA_FEATURE_DISJUNCTION,
    symbol: DISJUNCTION_UNICODE_SYMBOL,
    mask: 4,
};

pub const FEATURE_CONSTRAINTS: Feature = Feature {
    label: PRAGMA_FEATURE_CONSTRAINTS,
    symbol: FALSE_UNICODE_SYMBOL,
    mask: 8,
};

///
/// This value is a list of all supported features.
///
pub const ALL_FEATURES: &[&Feature] = &[
    &FEATURE_NEGATION,
    &FEATURE_COMPARISONS,
    &FEATURE_CONSTRAINTS,
    &FEATURE_DISJUNCTION,
];

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

const DEFAULT_FEATURE_SET: u8 = 0;

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Feature {
    pub fn label(&self) -> &'static str {
        self.label
    }

    pub fn symbol(&self) -> &'static str {
        self.symbol
    }
}

// ------------------------------------------------------------------------------------------------

impl Default for FeatureSet {
    fn default() -> Self {
        Self(DEFAULT_FEATURE_SET)
    }
}

impl From<Feature> for FeatureSet {
    fn from(v: Feature) -> Self {
        Self::from(vec![v])
    }
}

impl From<Vec<Feature>> for FeatureSet {
    fn from(vs: Vec<Feature>) -> Self {
        Self::from(vs.as_slice())
    }
}

impl From<&[Feature]> for FeatureSet {
    fn from(vs: &[Feature]) -> Self {
        Self(vs.iter().fold(0, |mut bits, feat| {
            bits |= feat.mask;
            bits
        }))
    }
}

impl Display for FeatureSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            if !self.is_default() {
                let features = self
                    .features()
                    .map(|feat| feat.label().to_string())
                    .collect::<Vec<String>>();

                format!(
                    "{}{}{}{}{}{}",
                    RESERVED_PREFIX,
                    RESERVED_PRAGMA_FEATURE,
                    CHAR_LEFT_PAREN,
                    features.join(&format!("{} ", CHAR_COMMA)),
                    CHAR_RIGHT_PAREN,
                    CHAR_PERIOD,
                )
            } else {
                String::new()
            }
        )
    }
}

impl FeatureSet {
    pub fn is_default(&self) -> bool {
        self.0 == DEFAULT_FEATURE_SET
    }

    pub fn all() -> Self {
        Self::from(ALL_FEATURES.to_vec())
    }

    pub fn supports(&self, feature: &Feature) -> bool {
        self.0 & feature.mask == feature.mask
    }

    pub fn add_support_for(&mut self, feature: &Feature) -> Self {
        self.0 |= feature.mask;
        *self
    }

    pub fn remove_support_for(&mut self, feature: &Feature) -> Self {
        self.0 &= !feature.mask;
        *self
    }

    pub fn features(&self) -> impl Iterator<Item = &Feature> {
        ALL_FEATURES
            .iter()
            .filter(|feat| self.supports(feat))
            .cloned()
    }

    pub fn label(&self) -> String {
        if self.is_default() {
            DEFAULT_LANGUAGE_NAME.to_owned()
        } else {
            format!(
                "{}{}{}{}",
                CHAR_LEFT_PAREN,
                self.features()
                    .map(|feat| feat.symbol().to_string())
                    .collect::<String>(),
                CHAR_RIGHT_PAREN,
                DEFAULT_LANGUAGE_NAME,
            )
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_label() {
        assert_eq!(FeatureSet::default().label(), "Datalog");
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .label(),
            "(￢)Datalog"
        );
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .add_support_for(&FEATURE_DISJUNCTION)
                .label(),
            "(￢∨)Datalog"
        );
    }

    #[test]
    fn test_to_string() {
        assert_eq!(FeatureSet::default().to_string(), "");
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .to_string(),
            "@feature(negation)."
        );
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .add_support_for(&FEATURE_DISJUNCTION)
                .to_string(),
            "@feature(negation, disjunction)."
        );
    }
}
