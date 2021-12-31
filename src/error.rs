/*!
This module containing the `Error` and `Result` types.
 */

use crate::edb::Predicate;
use crate::features::Feature;
use crate::parse::SourceLocation;
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types
// ------------------------------------------------------------------------------------------------

///
/// The type for all errors returned from this library.
///
#[derive(Debug)]
pub enum Error {
    FileIoError(std::io::Error),
    FormatError(std::fmt::Error),
    ParserError(Box<dyn std::error::Error>),
    TypeMismatch(String, String),
    InvalidValue(String, String),
    InvalidPragmaName(String),
    InvalidPragmaArgumentCount(usize, usize),
    InvalidPragmaArgumentType(String),
    LanguageFeatureDisabled(Feature),
    LanguageFeatureUnsupported(Feature),
    UnknownLanguageFeature(String),
    ExtensionalPredicateInRuleHead(Predicate, Option<SourceLocation>),
    InvalidHeadCount(usize, usize, usize, Option<SourceLocation>),
    HeadVariablesMissingInBody(String, Option<SourceLocation>, Vec<String>),
    NegativeVariablesNotPositive(String, Option<SourceLocation>, Vec<String>),
    RelationExists(Predicate),
    RelationDoesNotExist(Predicate),
    FactDoesNotConformToSchema(Predicate, String),
}

///
/// The result of operations where the error returned is `asdi::error::Error`.
///
pub type Result<T> = std::result::Result<T, Error>;

// ------------------------------------------------------------------------------------------------
// Implementations
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
                Self::TypeMismatch(expecting, given) => format!(
                    "Type mismatch, expecting {:?}, given {:?}",
                    expecting, given
                ),
                Self::InvalidValue(expecting, given) =>
                    format!("Invalid value for type {}, given {:?}", expecting, given),
                Error::InvalidPragmaName(name) =>
                    format!("Invalid name, '{}' is not a pragma", name),
                Error::InvalidPragmaArgumentCount(expecting, given) => format!(
                    "Invalid number of arguments for pragma, expecting {}, given {}",
                    expecting, given
                ),
                Error::InvalidPragmaArgumentType(expecting) => format!(
                    "Invalid type for pragma argument, expecting a {}",
                    expecting
                ),
                Error::UnknownLanguageFeature(name) =>
                    format!("unknown or unsupported language feature, '{}'", name),
                Error::LanguageFeatureDisabled(feature) =>
                    format!("The language feature {} is not enabled.", feature.label()),
                Error::LanguageFeatureUnsupported(feature) =>
                    format!("The language feature {} is not supported by the attempted operation.", feature.label()),
                Error::ExtensionalPredicateInRuleHead(name, loc) => format!("A predicate {} from the EDB was present in a rule head{}.", name, match loc {
                    None => String::new(),
                    Some(src) => format!(" (at {})", src),
                }),
                Error::InvalidHeadCount(count, min, max, loc) =>
                    format!(
                        "Rule{} has invalid number of head atoms, {}, for current feature set. expecting {}..{}",
                        match loc {
                            None => String::new(),
                            Some(src) => format!(" (at {})", src),
                        },
                        count,
                        min,max,
                    ),
                Error::HeadVariablesMissingInBody(rule, loc, vars) =>
                    format!(
                        "In rule '{}'{}, the variables '{}' in the head do not appear in any positive literal in the body.",
                        rule,
                        match loc {
                            None => String::new(),
                            Some(src) => format!(" (at {})", src),
                        },
                        vars.join(", ")
                    ),
                Error::NegativeVariablesNotPositive(rule, loc, vars) =>
                    format!(
                        "In rule '{}'{}, the variables '{}' in some negative literals do not appear in any positive literal in the body.",
                        rule,
                        match loc {
                            None => String::new(),
                            Some(src) => format!(", at {}", src),
                        },
                        vars.join(", ")
                    ),
                Error::RelationExists(predicate) => format!("The relation '{}' already exists in the extensional database.", predicate),
                Error::RelationDoesNotExist(predicate) => format!("The relation '{}' does not exist in the selected database.", predicate),
                Error::FactDoesNotConformToSchema(predicate, terms) => format!("The fact values ({}) do not meet the schema requirements for relation '{}'", terms, predicate),
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
