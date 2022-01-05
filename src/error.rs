/*!
This module provides the common `Error` and `Result` types for this library.
 */

use crate::edb::Predicate;
use crate::features::Feature;
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types
// ------------------------------------------------------------------------------------------------

///
/// A line/column location within a source file, used for error reporting.
///
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceLocation {
    line: usize,
    column: usize,
}

///
/// The type for all errors returned from this library. In the case of `ParserError` the
/// implementation of display will use the underlying Pest error and so give a nicely
/// formatted response.
///
#[derive(Debug)]
pub enum Error {
    /// A wrapper around an underlying [`std::io::Error`].
    FileIoError(std::io::Error),

    /// A wrapper around an underlying [`std::fmt::Error`].
    FormatError(std::fmt::Error),

    /// A wrapper around an underlying [`std::error::Error`] denoting a Pest parser error.
    ParserError(Box<dyn std::error::Error>),

    /// A wrapper around any underlying serialization error.
    Serialization(Box<dyn std::error::Error>),

    /// A serialization string in an `@input` or `@output` pragma is not a supported
    /// serialization format.
    SerializationFormatUnknown {
        format: String,
    },

    /// Either serialization, or deserialization, operation is not supported by the serialization
    /// format.
    SerializationOperationUnsupported {
        format: String,
    },

    /// An operation cannot be performed with this feature disabled.
    LanguageFeatureDisabled {
        feature: Feature,
    },

    /// An operation cannot be performed with this feature enabled.
    LanguageFeatureUnsupported {
        feature: Feature,
    },

    /// The value is not a valid representation for the expected type.
    InvalidValue {
        expecting_type: String,
        given_value: String,
    },

    FactDoesNotConformToSchema {
        label: Predicate,
        terms: String,
    },

    ExtensionalPredicateInRuleHead {
        label: Predicate,
        location: Option<SourceLocation>,
    },

    InvalidHeadAtomCount {
        actual: usize,
        min: usize,
        max: usize,
        location: Option<SourceLocation>,
    },

    HeadVariablesMissingInBody {
        atom: Predicate,
        variables: Vec<String>,
        location: Option<SourceLocation>,
    },

    NegativeVariablesNotAlsoPositive {
        atom: Predicate,
        variables: Vec<String>,
        location: Option<SourceLocation>,
    },

    RelationExists {
        label: Predicate,
    },

    RelationDoesNotExist {
        label: Predicate,
    },
}

///
/// The result of operations where the error returned is `asdi::error::Error`.
///
pub type Result<T> = std::result::Result<T, Error>;

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

#[inline]
pub fn serialization_format_unknown<S: Into<String>>(format: S) -> Error {
    Error::SerializationFormatUnknown {
        format: format.into(),
    }
}

#[inline]
pub fn serialization_operation_unsupported<S: Into<String>>(format: S) -> Error {
    Error::SerializationOperationUnsupported {
        format: format.into(),
    }
}

#[inline]
pub fn language_feature_disabled(feature: Feature) -> Error {
    Error::LanguageFeatureDisabled { feature }
}

#[inline]
pub fn language_feature_unsupported(feature: Feature) -> Error {
    Error::LanguageFeatureUnsupported { feature }
}

#[inline]
pub fn invalid_value<S: Into<String>>(expecting_type: S, given_value: S) -> Error {
    Error::InvalidValue {
        expecting_type: expecting_type.into(),
        given_value: given_value.into(),
    }
}

#[inline]
pub fn fact_does_not_correspond_to_schema<S: Into<String>>(label: Predicate, terms: S) -> Error {
    Error::FactDoesNotConformToSchema {
        label,
        terms: terms.into(),
    }
}

#[inline]
pub fn extensional_predicate_in_rule_head(
    label: Predicate,
    location: Option<SourceLocation>,
) -> Error {
    Error::ExtensionalPredicateInRuleHead { label, location }
}

#[inline]
pub fn invalid_head_atom_count(
    actual: usize,
    min: usize,
    max: usize,
    location: Option<SourceLocation>,
) -> Error {
    Error::InvalidHeadAtomCount {
        actual,
        min,
        max,
        location,
    }
}

#[inline]
pub fn head_variables_missing_in_body(
    atom: Predicate,
    variables: Vec<String>,
    location: Option<SourceLocation>,
) -> Error {
    Error::HeadVariablesMissingInBody {
        atom,
        variables,
        location,
    }
}

#[inline]
pub fn negative_variables_not_also_positive(
    atom: Predicate,
    variables: Vec<String>,
    location: Option<SourceLocation>,
) -> Error {
    Error::NegativeVariablesNotAlsoPositive {
        atom,
        variables,
        location,
    }
}

#[inline]
pub fn relation_exists(label: Predicate) -> Error {
    Error::RelationExists { label }
}

#[inline]
pub fn relation_does_not_exist(label: Predicate) -> Error {
    Error::RelationDoesNotExist { label }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl From<(usize, usize)> for SourceLocation {
    fn from(v: (usize, usize)) -> Self {
        Self {
            line: v.0,
            column: v.1,
        }
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}, column {}]", self.line, self.column)
    }
}

impl SourceLocation {
    /// The line number (0-based) at which the error occurred.
    pub fn line(&self) -> usize {
        self.line
    }

    /// The column, or character, offset (0-based) at which the error occurred.
    pub fn column(&self) -> usize {
        self.column
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::FileIoError(e) => format!("File IO error, {}", e),
                Self::FormatError(e) => format!("Formatting error, {}", e),
                Self::ParserError(e) => e.to_string(),
                Self::InvalidValue { expecting_type, given_value } =>
                    format!("Invalid value for type {}, given {:?}", expecting_type, given_value),
                Error::LanguageFeatureDisabled{ feature } =>
                    format!("The language feature {} is not enabled.", feature.label()),
                Error::LanguageFeatureUnsupported{ feature } =>
                    format!("The language feature {} is not supported by the attempted operation.", feature.label()),
                Error::ExtensionalPredicateInRuleHead { label, location } =>
                    format!("A predicate {} from the EDB was present in a rule head{}.", label, match location {
                        None => String::new(),
                        Some(src) => format!(" (at {})", src),
                    }),
                Error::InvalidHeadAtomCount { actual, min, max, location } =>
                    format!(
                        "Rule{} has invalid number of head atoms, {}, for current feature set. expecting {}..{}",
                        match location {
                            None => String::new(),
                            Some(src) => format!(" (at {})", src),
                        },
                        actual,
                        min,max,
                    ),
                Error::HeadVariablesMissingInBody { atom, variables, location } =>
                    format!(
                        "In rule '{}'{}, the variables '{}' in the head do not appear in any positive literal in the body.",
                        atom,
                        match location {
                            None => String::new(),
                            Some(src) => format!(" (at {})", src),
                        },
                        variables.join(", ")
                    ),
                Error::NegativeVariablesNotAlsoPositive{ atom, variables, location } =>
                    format!(
                        "In rule '{}'{}, the variables '{}' in some negative literals do not appear in any positive literal in the body.",
                        atom,
                        match location {
                            None => String::new(),
                            Some(src) => format!(", at {}", src),
                        },
                        variables.join(", ")
                    ),
                Error::RelationExists { label } => format!("The relation '{}' already exists in the extensional database.", label),
                Error::RelationDoesNotExist { label } => format!("The relation '{}' does not exist in the selected database.", label),
                Error::FactDoesNotConformToSchema { label, terms } => format!("The fact values ({}) do not meet the schema requirements for relation '{}'", terms, label),
                Error::Serialization(e) => format!("An error occured either serializing or deserializing a relation: {}", e),
                Error::SerializationFormatUnknown { format: serialization } => format!("'{}' is not a supported serialization format.", serialization),
                Error::SerializationOperationUnsupported{ format: serialization } => format!("The requested I/O operation is not supported by the serialization format '{}'", serialization),
            }
        )
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Self::FileIoError(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::FileIoError(e)
    }
}

impl From<std::fmt::Error> for Error {
    fn from(e: std::fmt::Error) -> Self {
        Self::FormatError(e)
    }
}

impl<T> From<Error> for Result<T> {
    fn from(v: Error) -> Self {
        Err(v)
    }
}

impl Error {}
