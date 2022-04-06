/*!
TBD
 */

use crate::edb::Attribute;
use crate::idb::query::relational::Criteria;
use crate::idb::Variable;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait Union<Rhs = Self> {
    type Output;

    fn union(self, rhs: Rhs) -> Self::Output;
}

pub trait Intersect<Rhs = Self> {
    type Output;

    fn intersect(self, rhs: Rhs) -> Self::Output;
}

pub trait Difference<Rhs = Self> {
    type Output;

    fn difference(self, rhs: Rhs) -> Self::Output;
}

pub trait CartesianProduct<Rhs = Self> {
    type Output;

    fn cartesian_product(self, rhs: Rhs) -> Self::Output;
}

pub trait Select<V: Into<Vec<Criteria>>> {
    type Output;

    fn select(self, criteria: V) -> Self::Output;
}

pub trait Project<V: Into<Vec<Attribute<Variable>>>> {
    type Output;

    fn project(self, indices: V) -> Self::Output;
}

pub trait NaturalJoin<Rhs = Self> {
    type Output;

    fn natural_join(self, rhs: Rhs) -> Self::Output;
}

pub trait ThetaJoin<V: Into<Vec<Criteria>>, Rhs = Self> {
    type Output;

    fn theta_join(self, criteria: V, rhs: Rhs) -> Self::Output;
}
