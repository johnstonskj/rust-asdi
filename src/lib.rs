/*!
Another Simplistic [Datalog](https://en.wikipedia.org/wiki/Datalog) Implementation (ASDI), in Rust.

This package provides a data model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog)
programs in memory, a parser for the textual representation, and some evaluation implementations.

The text representation parser is a separate feature, so if you only need to construct and evaluate
programs using the API you may opt out of the [Pest](https://pest.rs) parser and support.

# Datalog Defined

Datalog is a logic programming language and a subset of the earlier
[Prolog](https://en.wikipedia.org/wiki/Prolog).

When referring to the specifics of the language we will use the common format $\text{\small{Datalog}}$ with
superscripts that identify specific language extensions; for example, $\small\text{Datalog}^{\lnot}$ is
the language extended with negation of literals, $\small\text{Datalog}^{\Gamma}$ is the language
extended with type checking on attributes, and $\small\text{Datalog}^{\lnot,=}$. is the language
extended with negation of literals _and_ comparison expressions. The order of superscript symbols is
irrelevant.

## Abstract Syntax

**Rules** $\small R$ are built from a language $\small \mathcal{L}=\( \mathcal{C},\mathcal{P},\mathcal{V}\)$
that contains the

1. $\small \mathcal{C}$ -- the finite sets of symbols for all constants; e.g. `hello`, `"hi"`
   `123`,
2. $\small \mathcal{P}$ -- the finite set of alphanumeric character strings that begin with a
   lowercase character; e.g. `human`, `size`, `a`,
3. $\small \mathcal{V}$ -- the finite set of alphanumeric character strings that begin with an
   uppercase character; e.g. `X`, `A`, `Var`.

Each rule $\small r \in R$ has the form:

$$\tag{i}\small A_1, \ldots, A_m \leftarrow L_1, \ldots, L_n$$

as well as the following properties:

1. $\small head(r)$ (consequence), returns the set of _atom_ values $\small A_1, \ldots, A_m$ where $\small m \in \mathbb{N}$,
2. $\small body(r)$ (antecedence), returns the set of _literal_ values $\small L_1, \ldots, L_n$ where $\small n \in \mathbb{N}$,
1. $\small distinguished(r)$ returns the set of terms in the head of a rule,
   $$\tag{ii}\small distinguished(r) \leftarrow \lbrace t | t \in \bigcup\lbrace terms(a) | a \in head(r) \rbrace \rbrace$$
1. $\small non\text{-}distinguished(r)$ returns the set of terms in the body that of a rule that are not in the head,
   $$\tag{iii}\small non\text{-}distinguished(r) \leftarrow \lbrace t | t \in \( \bigcup\lbrace terms(a) | a \in body(r) \rbrace - distinguished(r) \rbrace\)$$
3. $\small ground(r)$  returns true if its head _and_ its body are both ground:
   $$\tag{iv}\small ground\(r\) \leftarrow \(\forall{a}\in head\(r\); ground\(a\)\) \land \(\forall{l}\in body\(r\); ground\(l\)\)$$
4. $\small positive(r)$ returns true if all body literals are positive:
   $$\tag{v}\small positive(r) \leftarrow \(\forall{l}\in body\(r\); positive(l\)\)$$

A _pure_ rule is one where there is only a single atom in the head; if the body is true, the head is
true. A _constraint_ rule, or contradiction, does not allow any consequence to be determined from
evaluation of its body. A _disjunctive_ rule is one where there is more than one atom, and any one
may be true if the body is true. The property $\small form(r)$ returns the form of the rule, based
on the cardinality of the rule's head as follows:

$$\tag{vi}\small form(r) = \begin{cases} pure, &\text{if } |head\(r\)| = 1 \\\\ constraint, &\text{if } |head\(r\)| = 0 \land \text{Datalog}^{\lnot} \\\\ disjunctive, &\text{if } |head\(r\)| > 1  \land \text{Datalog}^{\lor}\end{cases}$$

**Terms**, mentioned above, may be constant values or variables such that
$\small\mathcal{T}=\mathcal{C}\cup\mathcal{V}\cup\bar{t}$ where $\small\bar{t}$ represents an
unused or unnamed variable. $\small\bar{t}$ is denoted in the text representation as an underscore
`"_"`).

With the definition of rules as they stand it is possible to write rules that generate an an
infinite number of results. To avoid such problems Datalog rules are required to satisfy the
following **Safety** conditions:

1. Every variable that appears in the head of a clause also appears in a positive literal in the
   body of the clause.
   $$\tag{vii}\small \lbrace t | t \in distinguished(r), t \in \mathcal{V} \rbrace - \lbrace t | t \in \bigcup\lbrace terms(a) | a \in body(r), positive(a) \rbrace, t \in \mathcal{V} \rbrace = \empty$$
2. Every variable appearing in a negative literal in the body of a clause also appears in some
   positive literal in the body of the clause.
   $$\tag{viii}\small \begin{aligned}\lbrace t | t \in \bigcup\lbrace terms(a) | a \in body(r), \lnot positive(a) \rbrace, t \in \mathcal{V} \rbrace - \newline \lbrace t | t \in \bigcup\lbrace terms(a) | a \in body(r), positive(a) \rbrace, t \in \mathcal{V} \rbrace = \empty\end{aligned}$$

**Atoms** are comprised of a label, $\small p$, and a tuple of terms. A set of atoms sharing the same
label comprise a _relation_ labeled $\small p$. The form of an individual predicate is as follows:

$$\tag{ix}\small p\(t_1, \ldots, t_k\)$$

as well as the following properties:

1. a predicate, $\small p \in \mathcal{P}$, that labels the relation,
1. $\small terms(a)$ returns the tuple of term values $\small t_1, \ldots, t_k$; where
   $\small t \in \mathcal{T}$ and $\small k \in \mathbb{N}^{+}$,
1. $\small arity(a)$ returns the cardinality of the relation identified by the predicate;
   $\small arity(a) \equiv |terms(a)| \equiv k$,
1. in $\small\text{Datalog}^{\Gamma}$:
   1. there exists a type environment $\small \Gamma$ consisting of one or more types $\small \tau$,
   1. each term $\small t_i$ has a corresponding type  $\small \tau_i$ where $\small \tau \in \Gamma$,
   1. $\small type(t)$ returns the type $\small \tau$ for that term,
   1. $\small types(a)$ returns a tuple such that;
      $\small \(i \in \{1, \ldots, arity(a)\} | type(t_i)\)$,
1. $\small ground(r)$ returns true if its terms are all constants:
   $$\tag{x}\small ground\(a\) \leftarrow \(\forall{t}\in terms\(a\); t \in \mathcal{C}\)$$

To visualize a set of facts in a relational form we take may create a table $p$, where each column,
or attribute, corresponds to a term index $1 \ldots k$. If the facts are typed then each column
takes on the corresponding $\tau$ type. Finally each row in the table is populated with the tuple
of term values.

|                 | $\small col_1: \tau_1$ | $\small \ldots$ | $\small col_k: \tau_k$ |
| --------------- | ---------------------- | --------------- | ---------------------- |
| $\small row_1$  | $\small t_{1_1}$       | $\small \ldots$ | $\small t_{1_k}$       |
| $\small \ldots$ | $\small \ldots$        | $\small \ldots$ | $\small \ldots$        |
| $\small row_y$  | $\small t_{y_1}$       | $\small \ldots$ | $\small t_{y_k}$       |

**Literals**, present in the body of a rule, represent clauses that are the required to be true
for the rule to be considered true.

1. a literal may be an atom or, in $\small\text{Datalog}^{=}$, a conditional expression,
1. a conditional has the form $\small \langle t_{lhs} \text{ op } t_{rhs} \rangle$, where
   1. $\small \text{op } \in \lbrace =, \neq, <, \leq, >, \geq \rbrace$,
   1. in $\small\text{Datalog}^{\Gamma}$ both $\small t_{lhs}$ and $\small t_{rhs}$ terms have
      corresponding types $\small \tau_{lhs}$ and $\small \tau_{rhs}$,
   1. the types $\small \tau_{lhs}$ and $\small \tau_{rhs}$ **must** be _compatible_, for some
      system-defined definition of the property $\small compatible(\tau_{lhs}, \tau_{rhs}, \text{op})$,
1. in $\small\text{Datalog}^{\lnot}$ a literal may be negated, appearing as $\small \lnot l$,
1. has the following properties:
   1. $\small terms(l)$ returns either the set of terms in either the atom or comparison,
      $$\tag{xi}\small terms(l) = \begin{cases} terms(l), &\text{if } atom(l) \\\\ \lbrace t_{lhs}, t_{rhs} \rbrace, &\text{if } comparison(l) \land \text{Datalog}^{\=}\end{cases}$$
   1. $\small ground(l)$ returns true if its terms are all constants $\small \(\forall{t}\in terms\(l\); t \in \mathcal{C}\)$,
   1. $\small positive(l)$ in $\small\text{Datalog}^{\lnot}$ returns false if negated,
      otherwise it will always return true.

Any ground rule where $\small m=1$ and where $\small n=0$ is termed a **Fact** as it is true by
nature of having an empty body, or alternatively we may consider the body be comprised of the truth
value $\small\top$.

$$\tag{xii}\small fact(r) \leftarrow \(ground\(r\) \land form\(r\)=pure \land body\(r\)=\empty\)$$

An atom may be also used as a **Goal** or **Query** clause in that its constant and variable terms
may be used to match facts from the known facts or those that may be inferred from the set of rules
introduced. A ground goal is simply determining that any fact exists that matches all of the
constant values provided and will return true or false $\small existential(q) \leftarrow ground(q)$.
In the case that one or more variables exist a set of facts will be returned that match the
expressed constants and provide the corresponding values for the variables.

The set of facts (ground atoms) known a-priori is termed the **Extensional** database, EDB, or $\small D_E$,.
The set of rules, and any inferred facts, are termed the **Intensional** database, IDB, or $\small D_I$.

A Datalog **Program** $\small P$ is a tuple comprising the extensional database $\small D_{E}$, the
intensional database $\small D_{I}$, and a set of queries.

$$\tag{xiii}\small P=\( D_E, D_I, Q \)$$

This implies, at least, that the set of predicates accessible to queries in the program is the union
of predicates in the extensional and intensional databases.

$$\tag{xiv}\small \mathcal{P}_P = \mathcal{P}_E \cup \mathcal{P}_I$$

It should be obvious that the same exists for constants and variables;
$\small \mathcal{C}_P = \mathcal{C}_E \cup \mathcal{C}_I$ and
$\small \mathcal{V}_P = \mathcal{V}_E \cup \mathcal{V}_I$.

Datalog does not, in general, allow the rules comprising the intensional database to infer new
values for predicates that exist in the extensional database. This may be expressed as follows,
although in the textual representation it is expressed as an error to define a rule with a head
predicate that exists in the extensional database.

$$\tag{xv}\small \mathcal{P}_E \cap \mathcal{P}_I = \empty$$

The same restriction is not required for constants in $\small \mathcal{C}_P$ or variables in
$\small \mathcal{V}_P$ which should be shared.

## Concrete Syntax

The definitions below use ABNF
([Augmented BNF for Syntax Specifications](https://datatracker.ietf.org/doc/html/rfc5234)) and
focus both on the concrete syntax as expressed in the text representation.
The ABNF definition is somewhat simplified from the grammar used in the ASDI parser although any
deviations do not significantly affect the meaning of the language.

### Programs

A program consists of a set of facts that comprise the extensional database, a list of rules that
comprise the intensional database, and possibly a set of queries to interrogate the result of any
reasoning performed over the program.

```abnf
program         = *[ fact ] *[ rule ] *[ query ]
```

A program consists of a single file containing facts, rules, and queries as well as any additional
files referenced via _pragmas_.

### Facts

Facts **must** be expressed in the form of stand-alone ground atoms and **must** appear before any
rules.

```abnf
fact            = predicate [ constant-list ] "."
predicate       = LC_ALPHA *[ ALPHA / DIGIT / "_" ]
constant-list   = "(" [ constant *[ "," constant ] ] ")"
```

The following demonstrates a simple fact denoting that the constant `brooke` representing some
individual is the parent of some individual represented by the constant `"Xerces"`.

```datalog
parent("Xerces", brooke).
```

### Constant Values

Constants are supported in three types, String, Integer, and Boolean. Whereas some definitions of
Datalog introduce an additional Identifier type, ASDI treats these as _short strings_ that can
safely be expressed without quotes; therefore, the values `xerces` and `"xerces"` are equivalent.

```abnf
constant        = string / integer / boolean
string          = short-string / quoted-string
short-string    = predicate [ ":" ALPHA *[ ALPHA / DIGIT / "_" ] ]
quoted-string   = DQUOTE ... DQUOTE
integer         = +DIGIT
boolean         = "@true" / "⊤" / "@false" / "⊥"
```

Boolean values may also be represented using `⊤` (down tack `\u{22a4}`) for true, and `⊥` (up tack
`\u{22a5}`) for false where this may improve readability.

### Rules

As facts are treated as separate from rules in the text representation there is no need for empty
bodies -- all rules **must** have at least one literal. Material implication may be written using
the Unicode character `⟵` (long leftwards arrow`\u{27f5}`).

```abnf
rule            = head implication body "."
head            = [ atom *[ disjunction atom ] | "⊥" ]
disjunction     = ";" / "|" / "OR" / "∨"
implication     = ":-" / "<-" / "⟵"
body            = literal-list
```

The following rules are all equivalent.

```datalog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) <- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Y).
```

The language feature `disjunction` corresponds to the language $\small\text{Datalog}^{\lor}$ and
allows multiple atoms to appear in the rule's head with the semantics that these are choices. This
syntax will not be accepted unless the feature is enabled.

For example, the following describes the rule that _if X is a parent then X is **either** a
father **or** mother_.

```datalog
@feature(disjunction).

father(X) ⋁ mother(X) :- parent(X).
```

The language feature `constraints` corresponds to the language $\small\text{Datalog}^{\bot}$ and
allows the specification of rules with no head. In this case the material implication symbol is
**required**, the falsum value is optional for readability, therefore the following rules are
equivalent.

```datalog
@feature(constraints).

:- alive(X) AND dead(X).
⊥ ⟵ alive(X) ∧ dead(X).
```

### Atoms

The text representation of an atom is a relatively simple translation from the abstract syntax
above.

```abnf
atom            = predicate term-list
term-list       = "(" term *[ "," term ] ")"
term            = variable / constant
variable        = named-variable / anon-variable
named-variable  = UC_ALPHA *[ ALPHA / DIGIT / "_" ]
anon-variable   = "_"
```

The following are all atoms.

```datalog
dead(julius_caesar).
emperor(julius_caesar, rome).
emperor(X, Y).
emperor(X, rome).
```

### Literals

Any valid atom is also a valid _positive_ literal. The syntax below also allows for _negative_
literals as well as comparison expressions as literals. Conjunction may be written with the Unicode
character `∧` (logical and `\u{2227}`).

```abnf
literal-list    = literal *[ conjunction literal ]
literal         = [ negation ] atom / comparison
negation        = "!" / "NOT" / "￢"
conjunction     = "," / "&" / "AND" / "∧"
```

The following rules are all equivalent.

```datalog
ancestor(X, Y) ⟵ parent(X, Z), ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) & ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) AND ancestor(Z, Y).
```

The language feature `negation` corresponds to the language $\small\text{Datalog}^{\lnot}$ and
allows the specification of negated literals. Negation may also be written using the Unicode
character `￢` (full-width not sign `\u{ffe2}`).

```datalog
@feature(negation).

alive(X) :- NOT dead(X).
```

### Comparisons

The language feature `comparisons` corresponds to the language $\small\text{Datalog}^{=}$ and
allows the use of comparison expressions. Comparisons take place between two literals and are
currently limited to a set of common operators.

```abnf
comparison      = term operator term
operator        = "=" / "!=" / "/=" / "≠" / "<" / "<=" / "≤" / ">" / ">=" / "≥"
```

The Unicode characters `≠` (not equal to `\u{2260}`), `≤` (less-than or equal to `\u{2264}`), and
`≥` (greater-than or equal to `\u{2265}`) may be substituted for the common comparison operators.

All comparison operations **must** be between terms of the some type, such that the property
_compatible_ introduce above is defined as:

$$\tag{xvi}\small compatible(\tau_{lhs}, \tau_{rhs}, \text{op}) \leftarrow \tau_{lhs} = \tau_{rhs}$$

Additionally, some operators are not present for all types, as shown in the table below.

| Type     | `=`, `≠`   | `<`, `≤`, `>`, `≥` |
| -------- | ---------- | ------------------ |
| String   | Yes        | Yes - lexical      |
| Integer  | Yes        | Yes                |
| Boolean  | Yes        | No                 |

The following is an example using comparison of some numeric attribute of the _car_ relation.

```datalog
@feature(comparisons).

antique(X) :- car(X, Y) AND Y > 50.
```

### Queries

A query is simply an atom, but one identified to the system as a goal with either the prefix `?-`
or the suffix `?`.

```abnf
query           = ( "?-" atom "." ) / ( atom "?" )
```

The following queries are equivalent and will return the value of the variable `X` for any facts in
the _ancestor_ relationship where the first attribute is the string value `"xerces"`.

```datalog
?- ancestor(xerces, X).
ancestor(xerces, X)?
```

When the value `_` is used in a query it denotes an attribute of the relation that has no meaning
in either the query or the response. For example, in the following query we ask for all values of
the _model_ attribute in the _car_ relation where the _make_ is "ford", and ignore the age entirely.

```datalog
@declare car(make: string, model: string, age: integer).

car("ford", Model, _)?
```

The results of this query would not include the age column:

```text
| Model      |
| ---------- |
| bronco     |
| edge       |
| escape     |
| escort     |
| expedition |
| explorer   |
| fiesta     |
| focus      |
| fusion     |
| mustang    |
| ...        |
```

### Pragmas

TBD.

```abnf
pragma          = declare / include / input / output

declare         = "@declare" predicate attribute-list "."
attribute-list  = "(" attribute-decl *[ "," attribute-decl ] ")"
attribute-decl  = [ predicate ":" ] attribute-type
attribute-type  = "boolean" / "integer" / "string"

include         = "@include" quoted-string "."

input           = "@input" "(" predicate "," quoted-string [ "," quoted-string ] ")" "."

output          = "@output" "(" predicate "," quoted-string [ "," quoted-string ] ")" "."
```

```datalog
@declare human(name: string).
@input(human, "data/humans.csv", "csv").
@output(mortal, "data/mortals.txt", "text").
```

### Comments

Comments in Datalog are identified by the `#` character and continue to the end of the line.

```datalog
## Here's a comment
?- ancestor(xerces, X). # and another
```

# Example

The following program is the classical syllogism example, in the text representation.

```datalog
human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
```

The following is the same example constructed via the ASDI library.

```rust
use asdi::Program;
use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Term, Variable};
use std::str::FromStr;

let mut syllogism = Program::default();
let p_human = Predicate::from_str("human").unwrap();

let human = syllogism
    .add_new_relation(p_human.clone(), vec![Attribute::string()])
    .unwrap();
human.add_as_fact(["Socrates".into()]).unwrap();

let var_x: Term = Variable::from_str("X").unwrap().into();

syllogism
    .add_new_rule(
        Predicate::from_str("mortal").unwrap(),
        [var_x.clone()],
        [Atom::new(p_human, [var_x]).into()],
    )
    .unwrap();

syllogism
    .add_new_query(Predicate::from_str("mortal").unwrap(), ["Socrates".into()])
    .unwrap();
```

The execution of this program will start with the goal query "_is Socrates mortal?_" and in
doing so will evaluate the necessary rule and derive the relation _mortal_. The result is a
boolean value denoting whether the goal is satisfied.

```text
@true
```

However, if we were to change the final query to replace the constant with a variable, as follows.

```datalog
?- mortal(X).
```

The program will select all matching (in this case all) facts from the _mortal_ relation.

```text
| X          |
| ---------- |
| "Socrates" |
```

*/

#![warn(
    unknown_lints,
    // ---------- Stylistic
    absolute_paths_not_starting_with_crate,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    macro_use_extern_crate,
    nonstandard_style, /* group */
    noop_method_call,
    rust_2018_idioms,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    // ---------- Future
    future_incompatible, /* group */
    rust_2021_compatibility, /* group */
    // ---------- Public
    missing_debug_implementations,
    // missing_docs,
    unreachable_pub,
    // ---------- Unsafe
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    // ---------- Unused
    unused, /* group */
)]
#![deny(
    // ---------- Public
    exported_private_dependencies,
    private_in_public,
    // ---------- Deprecated
    anonymous_parameters,
    bare_trait_objects,
    ellipsis_inclusive_range_patterns,
    // ---------- Unsafe
    deref_nullptr,
    drop_bounds,
    dyn_drop,
)]

use crate::edb::{Attribute, Predicate, Relation, Relations, Schema};
use crate::error::{Error, Result};
use crate::features::FeatureSet;
use crate::idb::{Atom, Literal, Query, Rule, Rules, Term, Variable, View};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program {
    features: FeatureSet,
    asserted: Relations,
    infer: Relations,
    rules: Rules,
    queries: HashSet<Query>,
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

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.features.is_default() {
            writeln!(f, "{}", self.features)?;
            writeln!(f)?;
        }

        if !self.asserted.is_empty() {
            for relation in self.asserted.iter() {
                writeln!(f, "{}", relation.to_schema_decl(true))?;
            }
            writeln!(f)?;
        }

        if !self.infer.is_empty() {
            for relation in self.infer.iter() {
                writeln!(f, "{}", relation.to_schema_decl(false))?;
            }
            writeln!(f)?;
        }

        for db in [&self.infer, &self.asserted] {
            for relation in db.iter() {
                if !relation.is_empty() {
                    for fact in relation.iter() {
                        writeln!(f, "{}", fact)?;
                    }
                    writeln!(f)?;
                }
            }
        }

        writeln!(f, "{}", self.rules)?;

        for query in self.queries() {
            writeln!(f, "{}", query)?;
        }

        Ok(())
    }
}

impl Program {
    pub fn new_with_features(features: FeatureSet) -> Self {
        Self {
            features,
            asserted: Default::default(),
            infer: Default::default(),
            queries: Default::default(),
            rules: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn features(&self) -> &FeatureSet {
        &self.features
    }

    pub(crate) fn features_mut(&mut self) -> &mut FeatureSet {
        &mut self.features
    }

    // --------------------------------------------------------------------------------------------

    pub fn extensional(&self) -> &Relations {
        &self.asserted
    }

    pub fn extensional_mut(&mut self) -> &mut Relations {
        &mut self.asserted
    }

    pub fn add_relation(&mut self, relation: Relation) {
        self.extensional_mut().add(relation)
    }

    pub fn add_new_relation<V: Into<Schema<Predicate>>>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<&mut Relation> {
        self.extensional_mut()
            .add_new_relation(predicate, schema.into())
    }

    // --------------------------------------------------------------------------------------------

    pub fn intensional(&self) -> &Relations {
        &self.infer
    }

    pub fn intensional_mut(&mut self) -> &mut Relations {
        &mut self.infer
    }

    /// intensional predicate symbols
    pub fn rules(&self) -> &Rules {
        &self.rules
    }

    pub fn make_new_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &self,
        head_predicate: Predicate,
        head_terms: H,
        body: B,
    ) -> Result<Rule> {
        Ok(Rule::new(Atom::new(head_predicate, head_terms), body))
    }

    pub fn add_new_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &mut self,
        head_predicate: Predicate,
        head_terms: H,
        body: B,
    ) -> Result<()> {
        let rule = self.make_new_rule(head_predicate, head_terms, body)?;
        self.add_rule(rule)
    }

    pub fn add_rule(&mut self, rule: Rule) -> Result<()> {
        rule.check_well_formed(self.features())?;

        for atom in rule.head() {
            if self.asserted.contains(atom.predicate()) {
                return Err(Error::ExtensionalPredicateInRuleHead(
                    atom.predicate().clone(),
                    atom.source_location().cloned(),
                ));
            } else if !self.infer.contains(atom.predicate()) {
                let mut schema = Vec::with_capacity(atom.arity());
                for term in atom.terms() {
                    match term {
                        Term::Variable(v) => schema.push(self.infer_attribute(v, &rule)),
                        Term::Constant(c) => schema.push(Attribute::from(c.kind())),
                    }
                }
                self.intensional_mut()
                    .add_new_relation(atom.predicate().clone(), schema)?;
            }
        }

        // TODO: validate?
        self.rules.add(rule);
        Ok(())
    }

    fn infer_attribute(&self, variable: &Variable, rule: &Rule) -> Attribute<Predicate> {
        let candidates: Vec<(&Predicate, usize)> = rule
            .literals()
            .filter_map(Literal::as_atom)
            .filter_map(|a| {
                a.terms()
                    .enumerate()
                    .filter_map(|(i, term)| term.as_variable().map(|var| (i, var)))
                    .find(|(_, var)| var == &variable)
                    .map(|(i, _)| (a.predicate(), i))
            })
            .collect();
        for (predicate, i) in candidates {
            if let Some(relation) = self.extensional().relation(predicate) {
                return relation.schema().get(i).unwrap().clone();
            }
        }
        Attribute::anonymous()
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_positive(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_positive())
    }

    pub fn is_linear(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_linear())
    }

    pub fn is_guarded(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_guarded())
    }

    pub fn is_frontier_guarded(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_frontier_guarded())
    }

    pub fn is_non_recursive(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_non_recursive())
    }

    // --------------------------------------------------------------------------------------------

    pub fn queries(&self) -> impl Iterator<Item = &Query> {
        self.queries.iter()
    }

    pub fn make_new_query<T: Into<Vec<Term>>>(
        &self,
        predicate: Predicate,
        terms: T,
    ) -> Result<Query> {
        Ok(Query::new(predicate, terms))
    }

    pub fn add_new_query<T: Into<Vec<Term>>>(
        &mut self,
        predicate: Predicate,
        terms: T,
    ) -> Result<bool> {
        let query = self.make_new_query(predicate, terms)?;
        self.add_query(query)
    }

    pub fn add_query(&mut self, query: Query) -> Result<bool> {
        // TODO: validation?
        Ok(self.queries.insert(query))
    }

    pub fn eval_query(&self, query: &Query) -> Result<View> {
        Ok(self.extensional().matches(query.as_ref()))
    }

    pub fn eval_queries(&self) -> Vec<(&Query, Result<View>)> {
        self.queries().map(|q| (q, self.eval_query(q))).collect()
    }

    // --------------------------------------------------------------------------------------------

    pub fn check_well_formed(&self, _features: &FeatureSet) -> Result<()> {
        let result: Result<()> = self
            .rules()
            .iter()
            .try_for_each(|r| r.check_well_formed(&self.features));
        result
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod error;

pub mod features;

pub mod edb;

pub mod idb;

// ------------------------------------------------------------------------------------------------
// Feature-gated Modules
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "parser")]
pub mod parse;

#[cfg(feature = "typeset")]
pub mod typeset;

// ------------------------------------------------------------------------------------------------
// Private Modules
// ------------------------------------------------------------------------------------------------

mod syntax;
