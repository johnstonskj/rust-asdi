/*!
One-line description.

More detailed description, with

# Example

*/

use crate::{
    DisplayExt, Environment, CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN,
    DEFAULT_LANGUAGE_NAME, DISJUNCTION_UNICODE_SYMBOL, NOT_UNICODE_SYMBOL, OPERATOR_ASCII_EQUAL,
    PRAGMA_FEATURE_DISJUNCTION, PRAGMA_FEATURE_NEGATION, PRAGMA_FEATURE_OPERATORS,
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

pub const FEATURE_OPERATORS: Feature = Feature {
    label: PRAGMA_FEATURE_OPERATORS,
    symbol: OPERATOR_ASCII_EQUAL,
    bit: 2,
};

pub const FEATURE_DISJUNCTION: Feature = Feature {
    label: PRAGMA_FEATURE_DISJUNCTION,
    symbol: DISJUNCTION_UNICODE_SYMBOL,
    bit: 4,
};

pub const ALL_FEATURES: &[&Feature] =
    &[&FEATURE_NEGATION, &FEATURE_OPERATORS, &FEATURE_DISJUNCTION];

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
        if self.is_default() {
            write!(f, "{}", DEFAULT_LANGUAGE_NAME)
        } else {
            write!(
                f,
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

impl DisplayExt for FeatureSet {
    fn to_extern_string(&self, _: &Environment) -> String {
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
    fn test_display() {
        assert_eq!(FeatureSet::default().to_string(), "Datalog");
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .to_string(),
            "(￢)Datalog"
        );
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .add_support_for(&FEATURE_DISJUNCTION)
                .to_string(),
            "(￢⋁)Datalog"
        );
    }

    #[test]
    fn test_extern_string() {
        assert_eq!(
            FeatureSet::default().to_extern_string(&Environment::default()),
            ""
        );
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .to_extern_string(&Environment::default()),
            "@feature(negation)."
        );
        assert_eq!(
            FeatureSet::default()
                .add_support_for(&FEATURE_NEGATION)
                .add_support_for(&FEATURE_DISJUNCTION)
                .to_extern_string(&Environment::default()),
            "@feature(negation, disjunction)."
        );
    }
}
