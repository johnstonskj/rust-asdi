/*!
This module provides the both a [Reader] and [Writer] implementation for JSON files.
 */

use crate::edb::{AttributeKind, Constant, Fact};
use crate::error::Error;
use crate::error::Result;
use crate::io::{Reader, Writer};
use crate::syntax::{TYPE_NAME_CONST_BOOLEAN, TYPE_NAME_CONST_INTEGER, TYPE_NAME_CONST_STRING};
use crate::{error, Collection, Labeled, Relation};
use serde_json::{from_reader, Number, Value};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Options {
    pretty_out: bool,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Json {}

/// The string used in the `.input` and `.output` pragmas to identify this format.
pub const PRAGMA_ID: &str = "json";

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

impl Options {
    pub fn pretty_printed() -> Self {
        Self { pretty_out: true }
    }

    pub fn plain_output() -> Self {
        Self { pretty_out: false }
    }
}

// ------------------------------------------------------------------------------------------------

impl Reader for Json {
    type Options = Options;

    fn read_from_with_options(
        &self,
        file_name: &Path,
        as_relation: &Relation,
        _: &Self::Options,
    ) -> Result<Relation> {
        let file = File::open(file_name)?;
        let reader = BufReader::new(file);
        let value: Value = from_reader(reader).map_err(|e| Error::Serialization(Box::new(e)))?;

        let mut new_relation = as_relation.clone_with_schema_only();

        let attribute_types: Vec<AttributeKind> = as_relation
            .schema()
            .iter()
            .map(|a| a.kind().unwrap_or(AttributeKind::String))
            .collect();
        let arity = attribute_types.len();

        let facts = value.as_array().unwrap();
        for fact in facts {
            let values: &Vec<Value> = fact.as_array().unwrap();
            assert_eq!(values.len(), arity);
            println!("{:?}", values);
            let constants: Result<Vec<Constant>> = values
                .iter()
                .enumerate()
                .map(|(i, v)| match attribute_types.get(i) {
                    Some(AttributeKind::String) => json_value_to_string(v),
                    Some(AttributeKind::Integer) => json_value_to_integer(v),
                    Some(AttributeKind::Boolean) => json_value_to_boolean(v),
                    _ => unreachable!(),
                })
                .collect();
            new_relation.add(Fact::new(new_relation.label_ref(), constants?))?;
        }

        Ok(new_relation)
    }
}

impl Writer for Json {
    type Options = Options;

    fn write_to_with_options(
        &self,
        file_name: &Path,
        relation: &Relation,
        options: &Self::Options,
    ) -> Result<()> {
        let mut file = File::create(file_name)?;
        let value: Value = relation_to_json(relation);
        if options.pretty_out {
            serde_json::to_writer_pretty(&mut file, &value).unwrap();
        } else {
            serde_json::to_writer(&mut file, &value).unwrap();
        }
        Ok(())
    }

    fn print_with_options(&self, relation: &Relation, options: &Self::Options) -> Result<()> {
        let value: Value = relation_to_json(relation);
        if options.pretty_out {
            serde_json::to_writer_pretty(&mut std::io::stdout(), &value).unwrap();
        } else {
            serde_json::to_writer(&mut std::io::stdout(), &value).unwrap();
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn relation_to_json(relation: &Relation) -> Value {
    let mut array: Vec<Value> = Vec::with_capacity(relation.len());

    for fact in relation.iter() {
        array.push(fact_to_json(fact));
    }

    Value::Array(array)
}

fn fact_to_json(fact: &Fact) -> Value {
    let mut array: Vec<Value> = Vec::with_capacity(fact.len());

    for constant in fact.iter() {
        array.push(match constant {
            Constant::String(v) => Value::String(v.to_owned()),
            Constant::Integer(v) => Value::Number(Number::from(*v)),
            Constant::Boolean(v) => Value::Bool(*v),
        });
    }
    Value::Array(array)
}

#[inline]
fn json_value_to_string(v: &Value) -> Result<Constant> {
    let v2: String = v
        .as_str()
        .ok_or_else(|| error::invalid_value(TYPE_NAME_CONST_STRING, &v.to_string()))?
        .to_owned();
    Ok(Constant::String(v2))
}

#[inline]
fn json_value_to_integer(v: &Value) -> Result<Constant> {
    let v2: i64 = v
        .as_i64()
        .ok_or_else(|| error::invalid_value(TYPE_NAME_CONST_INTEGER, &v.to_string()))?
        .to_owned();
    Ok(Constant::Integer(v2))
}

#[inline]
fn json_value_to_boolean(v: &Value) -> Result<Constant> {
    let v2: bool = v
        .as_bool()
        .ok_or_else(|| error::invalid_value(TYPE_NAME_CONST_BOOLEAN, &v.to_string()))?
        .to_owned();
    Ok(Constant::Boolean(v2))
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
