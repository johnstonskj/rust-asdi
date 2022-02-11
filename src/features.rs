/*!
This module provides the `Feature` and `FeatureSet` types that allow programs to support specific
extensions to Datalog.

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/features.svg)

*/

use crate::syntax::{
    CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, DEFAULT_LANGUAGE_NAME,
    FEATURE_COMPARISONS_ID, FEATURE_COMPARISONS_SYMBOL, FEATURE_CONSTRAINTS_ID,
    FEATURE_CONSTRAINTS_SYMBOL, FEATURE_DISJUNCTION_ID, FEATURE_DISJUNCTION_SYMBOL,
    FEATURE_NEGATION_ID, NEGATION_UNICODE_SYMBOL, PRAGMA_ID_FEATURE, RESERVED_PREFIX,
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

///
/// Corresponds to `.feature(negation).` and the language $\small\text{Datalog}^{\lnot}$, without
/// this feature it is an error to add a rule with a non-positive literal.
///
pub const FEATURE_NEGATION: Feature = Feature {
    label: FEATURE_NEGATION_ID,
    symbol: NEGATION_UNICODE_SYMBOL,
    mask: 1,
};

///
/// Corresponds to `.feature(comparisons).` and the language $\small\text{Datalog}^{\theta}$, without
/// this feature it is an error to add a rule with an arithmetic literal.
///
pub const FEATURE_COMPARISONS: Feature = Feature {
    label: FEATURE_COMPARISONS_ID,
    symbol: FEATURE_COMPARISONS_SYMBOL,
    mask: 2,
};

///
/// Corresponds to `.feature(disjunction).` and the language $\small\text{Datalog}^{\lor}$, without
/// this feature it is an error to add a rule with a disjunctive head.
///
pub const FEATURE_DISJUNCTION: Feature = Feature {
    label: FEATURE_DISJUNCTION_ID,
    symbol: FEATURE_DISJUNCTION_SYMBOL,
    mask: 4,
};

// pub const FEATURE_EXCLUSIVE_DISJUNCTION: Feature = Feature {
//     label: PRAGMA_FEATURE_EXCLUSIVE_DISJUNCTION,
//     symbol: FEATURE_SYMBOL_CIRCLE_PLUS,
//     mask: 8,
// };

///
/// Corresponds to `.feature(constraints).` and the language $\small\text{Datalog}^{\Leftarrow}$, without
/// this feature it is an error to add a rule without a head.
///
pub const FEATURE_CONSTRAINTS: Feature = Feature {
    label: FEATURE_CONSTRAINTS_ID,
    symbol: FEATURE_CONSTRAINTS_SYMBOL,
    mask: 16,
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
// Implementations
// ------------------------------------------------------------------------------------------------

impl Feature {
    ///
    /// Return the label (string used in the feature pragma) for this feature.
    ///
    pub fn label(&self) -> &'static str {
        self.label
    }

    ///
    /// Return the symbol (used as a language identifier) for this feature.
    ///
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
                    PRAGMA_ID_FEATURE,
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
    ///
    /// Returns `true` if this is the default (no language extension features) set, else `false`.
    ///
    pub fn is_default(&self) -> bool {
        self.0 == DEFAULT_FEATURE_SET
    }

    ///
    /// Construct a new feature set with all supported features enabled.
    ///
    pub fn all() -> Self {
        Self::from(vec![
            FEATURE_NEGATION,
            FEATURE_COMPARISONS,
            FEATURE_CONSTRAINTS,
            FEATURE_DISJUNCTION,
        ])
    }

    ///
    /// Returns `true` if this feature set includes the provided feature, else `false`.
    ///
    pub fn supports(&self, feature: &Feature) -> bool {
        self.0 & feature.mask == feature.mask
    }

    ///
    /// Add the provided feature to this feature set, and return self so that these calls
    /// may be chained.
    ///
    pub fn add_support_for(&mut self, feature: &Feature) -> Self {
        self.0 |= feature.mask;
        *self
    }

    ///
    /// Remove the provided feature from this feature set, and return self so that these calls
    /// may be chained.
    ///
    pub fn remove_support_for(&mut self, feature: &Feature) -> Self {
        self.0 &= !feature.mask;
        *self
    }

    ///
    /// Returns an iterator over all features supported in this set.
    ///
    pub fn features(&self) -> impl Iterator<Item = &Feature> {
        ALL_FEATURES
            .iter()
            .filter(|feat| self.supports(feat))
            .cloned()
    }

    ///
    /// Return a string constructed to identify the language denoted by this feature set.
    ///
    pub fn language(&self) -> String {
        if self.is_default() {
            DEFAULT_LANGUAGE_NAME.to_owned()
        } else {
            format!(
                "{}{}{}{}",
                CHAR_LEFT_PAREN,
                self.features()
                    .map(|feat| feat.symbol().to_string())
                    .collect::<Vec<String>>()
                    .join(&format!("{}", CHAR_COMMA)),
                CHAR_RIGHT_PAREN,
                DEFAULT_LANGUAGE_NAME,
            )
        }
    }
}
