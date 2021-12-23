/*!
One-line description.

More detailed description, with

# Example

```datalog
@features(negation, comparisons, disjunction).
```

## Negation

```datalog
@features(negation).
alive(X) :- person(X) AND NOT dead(X).
alive(X) :- person(X) ∧ ￢dead(X).
```

## Comparisons

```datalog
@features(comparisons).
old(X) :- age(X, Y) ∧ Y > 75.
```

## Disjunction

```datalog
@features(disjunction).
mother(X, Y) OR father(X, Y) :- parent(X, Y).
mother(X, Y) ∨ father(X, Y) :- parent(X, Y).
```


*/

use crate::syntax::{
    CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, DEFAULT_LANGUAGE_NAME,
    DISJUNCTION_UNICODE_SYMBOL, NOT_UNICODE_SYMBOL, OPERATOR_ASCII_EQUAL,
    PRAGMA_FEATURE_COMPARISONS, PRAGMA_FEATURE_DISJUNCTION, PRAGMA_FEATURE_NEGATION,
    RESERVED_PRAGMA_FEATURE, RESERVED_PREFIX,
};
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FeatureSet(u8);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Feature {
    label: &'static str,
    symbol: &'static str,
    bit: u8,
}

pub const FEATURE_NEGATION: Feature = Feature {
    label: PRAGMA_FEATURE_NEGATION,
    symbol: NOT_UNICODE_SYMBOL,
    bit: 1,
};

pub const FEATURE_COMPARISONS: Feature = Feature {
    label: PRAGMA_FEATURE_COMPARISONS,
    symbol: OPERATOR_ASCII_EQUAL,
    bit: 2,
};

pub const FEATURE_DISJUNCTION: Feature = Feature {
    label: PRAGMA_FEATURE_DISJUNCTION,
    symbol: DISJUNCTION_UNICODE_SYMBOL,
    bit: 4,
};

pub const ALL_FEATURES: &[&Feature] = &[
    &FEATURE_NEGATION,
    &FEATURE_COMPARISONS,
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
            bits |= feat.bit;
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

    pub fn supports(&self, feature: &Feature) -> bool {
        self.0 & feature.bit == feature.bit
    }

    pub fn add_support_for(&mut self, feature: &Feature) -> Self {
        self.0 |= feature.bit;
        *self
    }

    pub fn remove_support_for(&mut self, feature: &Feature) -> Self {
        self.0 &= !feature.bit;
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
            "(￢⋁)Datalog"
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
