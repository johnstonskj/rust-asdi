use crate::edb::{AttributeKind, Predicate};
use crate::syntax::{
    CHAR_COLON, CHAR_UNDERSCORE, RESERVED_BOOLEAN_FALSE, RESERVED_BOOLEAN_TRUE, RESERVED_PREFIX,
};
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    String(String),
    Integer(i64),
    Boolean(bool),
}

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl From<&str> for Constant {
    fn from(s: &str) -> Self {
        if !s.chars().any(|c| c.is_control()) {
            Self::String(s.to_owned())
        } else {
            panic!();
        }
    }
}

impl From<String> for Constant {
    fn from(v: String) -> Self {
        Self::from(v.as_str())
    }
}

impl From<i64> for Constant {
    fn from(v: i64) -> Self {
        Self::Integer(v)
    }
}

impl From<bool> for Constant {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::String(v) =>
                    if Self::is_identifier(v) {
                        v.to_string()
                    } else {
                        format!("{:?}", v)
                    },
                Self::Integer(v) => v.to_string(),
                Self::Boolean(v) => {
                    if *v {
                        format!("{}{}", RESERVED_PREFIX, RESERVED_BOOLEAN_TRUE)
                    } else {
                        format!("{}{}", RESERVED_PREFIX, RESERVED_BOOLEAN_FALSE)
                    }
                }
            }
        )
    }
}

impl Constant {
    pub fn kind(&self) -> AttributeKind {
        match self {
            Self::String(_) => AttributeKind::String,
            Self::Integer(_) => AttributeKind::Integer,
            Self::Boolean(_) => AttributeKind::Boolean,
        }
    }

    pub fn is_identifier(s: &str) -> bool {
        let parts = s.split(CHAR_COLON).collect::<Vec<&str>>();
        if parts.len() == 1 {
            Predicate::is_valid(s)
        } else if parts.len() == 2 {
            Predicate::is_valid(parts.get(0).unwrap())
                && Self::is_identifier_relaxed(parts.get(1).unwrap())
        } else {
            false
        }
    }

    fn is_identifier_relaxed(s: &str) -> bool {
        let mut chars = s.chars();
        (!s.is_empty())
            && chars.next().map(|c| c.is_alphabetic()).unwrap()
            && chars.all(|c| c.is_alphanumeric() || c == CHAR_UNDERSCORE)
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
