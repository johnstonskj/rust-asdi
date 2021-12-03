/*!
Module containing the `Error` type.
 */

use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum Error {
    FileIoError(std::io::Error),
    ParserError(Box<dyn std::error::Error>),
    TypeMismatch(String, String),
    InvalidValue(String, String),
    InvalidPragmaName(String),
    InvalidPragmaArgumentCount(usize, usize),
    InvalidPragmaArgumentType(String),
}

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

impl<T> From<Error> for Result<T> {
    fn from(v: Error) -> Self {
        Err(v)
    }
}

impl Error {}
