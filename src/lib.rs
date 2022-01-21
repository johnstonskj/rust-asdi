/*!
Another Simplistic [Datalog](https://en.wikipedia.org/wiki/Datalog) Implementation (ASDI), in Rust.

This package provides a data model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog)
programs in memory, a parser for the textual representation, and some evaluation implementations.

The text representation parser is a separate feature, so if you only need to construct and evaluate
programs using the API you may opt out of the [Pest](https://pest.rs) parser and support.

# Datalog Defined

Datalog is a logic programming language and a subset of the earlier
[Prolog](https://en.wikipedia.org/wiki/Prolog). Chapter 1 of [Logic Programming and
Databases](https://link.springer.com/book/10.1007/978-3-642-83952-8) provides a good overview of
the drawbacks of Prolog and the advantages of Datalog for certain tasks.

When referring to the specifics of the language we will use the common format $\text{\small{Datalog}}$ with
superscripts that identify specific language extensions; for example, $\small\text{Datalog}^{\lnot}$ is
the language extended with negation of literals, $\small\text{Datalog}^{\Gamma}$ is the language
extended with type checking on attributes, and $\small\text{Datalog}^{\lnot,\theta}$ is the language
extended with negation of literals _and_ arithmetic literals. The order of superscript symbols is
irrelevant. Additionally, text in **bold** indicates a key concept in the language while text in
_italics_ indicates a forward reference to such a concept.

## Abstract Syntax

### Rules

Rules $\small R$ are built from a language $\small \mathcal{L}=\( \mathcal{C},\mathcal{P},\mathcal{V}\)$
that contains the

1. $\small \mathcal{C}$ -- the finite sets of symbols for all constant values; e.g. `hello`, `"hi"`
   `123`,
2. $\small \mathcal{P}$ -- the finite set of alphanumeric character strings that begin with a
   lowercase character; e.g. `human`, `size`, `a`,
3. $\small \mathcal{V}$ -- the finite set of alphanumeric character strings that begin with an
   uppercase character; e.g. `X`, `A`, `Var`.

While it would appear that the values from $\small \mathcal{P}$ or $\small \mathcal{V}$ would
overlap, these values must remain distinct. For example, the value `human` is a valid predicate and
string constant but they have distinct types in ASDI that ensure they are distinct.

Each rule $\small r \in R$ has the form:

$$\tag{i}\small A_1, \ldots, A_m \leftarrow L_1, \ldots, L_n$$

as well as the following properties:

1. $\small head(r)$ (the consequence), returns the set of _atom_ values $\small A_1, \ldots, A_m$ where $\small m \in \mathbb{N}$,
2. $\small body(r)$ (the antecedence), returns the set of _literal_ values $\small L_1, \ldots, L_n$ where $\small n \in \mathbb{N}$,
1. $\small distinguished(r)$ returns the set of _terms_ in the head of a rule,
   $$\tag{ii}\small distinguished(r) \coloneqq \lbrace t | t \in \bigcup\lbrace terms(a) | a \in head(r) \rbrace \rbrace$$
1. $\small non\text{-}distinguished(r)$ returns the set of _terms_ in the body that of a rule that are not in the head,
   $$\tag{iii}\small non\text{-}distinguished(r) \coloneqq \lbrace t | t \in \( \bigcup\lbrace terms(a) | a \in body(r) \rbrace - distinguished(r) \rbrace\)\rbrace$$
3. $\small ground(r)$  returns true if its head and its body are both _ground_:
   $$\tag{iv}\small ground\(r\) \coloneqq \(\forall{a}\in head\(r\); ground\(a\)\) \land \(\forall{l}\in body\(r\); ground\(l\)\)$$
4. $\small positive(r)$ returns true if all body _literals_ are _positive_:
   $$\tag{v}\small positive(r) \coloneqq \(\forall{l}\in body\(r\); positive(l\)\)$$

A _pure_ rule is one where there is only a single atom in the head; if the body is true, the head is
true. A **constraint** rule, or contradiction, does not allow any consequence to be determined from
evaluation of its body. A **disjunctive** rule is one where there is more than one atom, and any one
may be true if the body is true. The language $\small\text{Datalog}^{\lor}$ allows for _inclusive_
disjunction, and while a language, $\small\text{Datalog}^{\oplus}$, exists for _exclusive_ disjunction
it is not implemented here.

The property $\small form(r)$ returns the form of the rule, based
on the cardinality of the rule's head as follows:

$$\tag{vi}\small
  form(r) \coloneqq
  \begin{cases}
    pure, &\text{if } |head\(r\)| = 1 \\\\
    constraint, &\text{if } |head\(r\)| = 0 \land \text{Datalog}^{\lnot} \\\\
    disjunctive, &\text{if } |head\(r\)| > 1  \land \text{Datalog}^{\lor}
  \end{cases}$$

Note that this notation is similar to that of a [_sequent_](https://en.wikipedia.org/wiki/Sequent).
Taking our definition of a rule, $\small A_1, \ldots, A_m \leftarrow L_1, \ldots, L_n$, and swap the
order of antecedence and consequence we get $\small L_1, \ldots, L_m \vdash A_1, \ldots, A_n$.
A pure rule is termed a _simple conditional assertion_, a constraint rule is termed an
_unconditional assertion_, and a disjunctive rule is termed a _sequent_ (or simply _conditional
assertion_).

Alternatively, some literature defines a rule in the following form:

$$\tag{ia}\small C_1 | C_2 | \ldots | C_n \leftarrow A_1, \ldots, A_m, \lnot B_1, \ldots, \lnot B_k$$

Where this form shows the expanded head structure according to $\small\text{Datalog}^{\lor}$, and the
set of negated literals according to $\small\text{Datalog}^{\lnot}$.

### Terms

Terms, mentioned above, may be constant values or variables such that
$\small\mathcal{T}=\mathcal{C}\cup\mathcal{V}\cup\bar{t}$ where $\small\bar{t}$ represents an
anonymous variable.

Terms have the following properties:

1. $\small constant\(t\)$ returns true if the term argument is a constant value.
1. $\small variable\(t\)$ returns true if the term argument is a variable.
1. $\small anonymous\(t\)$ returns true if the term argument is the anonymous variable, $\small\bar{t}$.

With the definition of rules so far it is possible to write rules that generate an an
infinite number of results. To avoid such problems Datalog rules are required to satisfy the
following **Safety** conditions:

1. Every variable that appears in the head of a clause also appears in a positive relational literal
   (atom) in the body of the clause.
   $$\tag{vii}\small
   \begin{alignat*}{2}
     safe\text{-}head\(r\) &\coloneqq &&\lbrace t | t \in distinguished(r), t \in \mathcal{V} \rbrace \\\\
     &- &&\lbrace t | t \in \bigcup\lbrace terms(a) | a \in body(r), atom(a), positive(a) \rbrace, t \in \mathcal{V} \rbrace \\\\
     &= &&\empty
     \end{alignat*}$$
2. Every variable appearing in a negative literal in the body of a clause also appears in some
   positive relational literal in the body of the clause.
   $$\tag{viii}\small
   \begin{alignat*}{2}
     safe\text{-}negatives\(r\) &\coloneqq &&\lbrace t | t \in \bigcup\lbrace terms(a) | a \in body(r), \lnot positive\(a\) \rbrace, t \in \mathcal{V} \rbrace \\\\
     &- &&\lbrace t | t \in \bigcup\lbrace terms(a) | a \in body(r), atom(a), positive(a) \rbrace, t \in \mathcal{V} \rbrace \\\\
     &= &&\empty
   \end{alignat*}$$

### Atoms

Atoms are comprised of a label, $\small p \in \mathcal{P}$, and a tuple of _terms_. A set of atoms
form a **Relation** if each _conforms to_ the schema of the relation. The form of an
individual atom is as follows:

$$\tag{ix}\small p\(t_1, \ldots, t_k\)$$

as well as the following properties:

1. $\small label\(a\)$ returns the predicate $\small p$,
1. $\small terms\(a\)$ returns the tuple of term values $\small t_1, \ldots, t_k$; where
   $\small t \in \mathcal{T}$ and $\small k \in \mathbb{N}^{+}$,
1. $\small arity\(a\)$ returns the cardinality of the relation identified by the predicate;
   $\small arity\(a\) \equiv |terms(a)| \equiv k$,
1. in $\small\text{Datalog}^{\Gamma}$:
   1. there exists a type environment $\small \Gamma$ consisting of one or more types $\small \tau$,
   1. each term $\small t_i$ has a corresponding type  $\small \tau_i$ where $\small \tau \in \Gamma$,
   1. $\small type\(t\)$ returns the type $\small \tau$ for that term,
   1. $\small types\(a\)$ returns a tuple such that;
      $\small \(i \in \{1, \ldots, arity(a)\} | type(t_i)\)$,
1. $\small ground(a)$ returns true if its terms are all constants:
   $$\tag{x}\small ground\(a\) \coloneqq \(\forall{t}\in terms\(a\); t \in \mathcal{C}\)$$

### Relations

Every relation $\small r$ has a schema that describes a set of attributes
$\small \lbrace \alpha_1, \ldots, \alpha_j \rbrace$, and each attribute may be named, and may in
$\small\text{Datalog}^{\Gamma}$ also have a type.

Relations have the following properties:

1. $\small label\(r\)$ returns the predicate $\small p$,
1. $\small schema\(r\)$ returns the set of attributes $\small \lbrace \alpha_1, \ldots, \alpha_j \rbrace$;
   where $\small k \in \mathbb{N}^{+}$,
1. $\small arity\(r\)$ returns the number of attributes in the relation's schema, and therefore all
   atoms within the relation; $\small arity\(r\) \equiv |schema(a)| \equiv j$.

Attributes have the following properties:

1. $\small label\(\alpha\)$ returns either the predicate label of the attribute, or $\small\bot$.
1. in $\small\text{Datalog}^{\Gamma}$:
   1. $\small type\(\alpha\)$ returns a type $\small \tau$ for the attribute, where $\small \tau \in \Gamma$, or $\small\bot$.

The following defines a binary function that determines whether an atom $\small a$ conforms to the
schema of a relationship $\small r$.

$$\tag{xi}\small
\begin{alignat*}{2}
  conforms\(a, r\) &\coloneqq &&ground\(a\) \\\\
  &\land &&label\(a\) = label\(r\) \\\\
  &\land &&arity\(a\) = arity\(r\) \\\\
  &\land &&\forall{i} \in \lbrace 1, \ldots, arity\(r\)\rbrace \medspace conforms\( a_{t_i}, r_{\alpha_i} \)
\end{alignat*}
$$
$$\tag{xii}\small
  conforms\(t, \alpha\) \coloneqq
  label\(t\) = label\(\alpha\) \land
  \tau_{t} = \tau{\alpha}
$$

Note that in relational algebra it is more common to use the term domain $\small D$ to denote a possibly
infinite set of values. Each attribute on a relation has a domain $\small D_i$ such that each ground
term is a value $\small d_i$ and the equivalent of $\small \tau_i \in \Gamma$ becomes
$\small d_i \in D_i$.

To visualize a set of facts in a relational form we take may create a table $p$, where each column,
or attribute, corresponds to a term index $1 \ldots k$. If the facts are typed then each column
takes on the corresponding $\tau$ type. Finally each row in the table is populated with the tuple
of term values.

|                 | $\small col_1: \tau_1$ | $\small \ldots$ | $\small col_k: \tau_k$ |
| --------------- | ---------------------- | --------------- | ---------------------- |
| $\small row_1$  | $\small t_{1_1}$       | $\small \ldots$ | $\small t_{1_k}$       |
| $\small \ldots$ | $\small \ldots$        | $\small \ldots$ | $\small \ldots$        |
| $\small row_y$  | $\small t_{y_1}$       | $\small \ldots$ | $\small t_{y_k}$       |

### Literals

Literals within the body of a rule, represent sub-goals that are the required to be true for the
rule's head to be considered true.

1. A literal may be an atom (termed a relational literal) or, in $\small\text{Datalog}^{\theta}$, a
   conditional expression (termed an arithmetic literal),
1. a an arithmetic literal has the form $\small \langle t_{lhs} \theta t_{rhs} \rangle$, where
   1. $\small \theta \in \lbrace =, \neq, <, \leq, >, \geq \rbrace$,
   1. in $\small\text{Datalog}^{\Gamma}$ both $\small t_{lhs}$ and $\small t_{rhs}$ terms have
      corresponding types $\small \tau_{lhs}$ and $\small \tau_{rhs}$,
   1. the types $\small \tau_{lhs}$ and $\small \tau_{rhs}$ **must** be _compatible_, for some
      system-dependent definition of the property $\small compatible(\tau_{lhs}, \tau_{rhs}, \theta)$,
1. in $\small\text{Datalog}^{\lnot}$ a literal may be negated, appearing as $\small \lnot l$,
1. and has the following properties:
   1. $\small relational\(l\)$ returns true if the literal argument is a relational literal.
   1. $\small arithmetic\(l\)$ returns true if the literal argument is a arithmetic literal.
   1. $\small terms\(l\)$ returns either the set of terms in a literal,
      $$\tag{xiii}\small
        terms(l) \coloneqq
        \begin{cases}
          terms(l), &\text{if } relational(l) \\\\
          \lbrace t_{lhs}, t_{rhs} \rbrace, &\text{if } arithmetic(l) \land \text{Datalog}^{\theta}
        \end{cases}$$
   1. $\small ground\(l\)$ returns true if its terms are all constants $\small \(\forall{t}\in terms\(l\); t \in \mathcal{C}\)$,
   1. $\small positive\(l\)$ in $\small\text{Datalog}^{\lnot}$ returns false if negated,
      otherwise it will always return true.

### Facts

Any ground rule where $\small m=1$ and where $\small n=0$ is termed a **Fact** as it is true by
nature of having an empty body, or alternatively we may consider the body be comprised of the truth
value $\small\top$.

$$\tag{xiv}\small fact(r) \coloneqq \(ground\(r\) \land form\(r\)=pure \land body\(r\)=\empty\)$$

### Queries

An atom may be also used as a **Goal** or **Query** clause in that its constant and variable terms
may be used to match facts from the known facts or those that may be inferred from the set of rules
introduced. A ground goal is simply determining that any fact exists that matches all of the
constant values provided and will return true or false.
In the case that one or more variables exist a set of facts will be returned that match the
expressed constants and provide the corresponding values for the variables.

The set of facts (ground atoms) known a-priori is termed the **Extensional** database, EDB, or $\small D_E$,.
The set of rules, and any inferred facts, are termed the **Intensional** database, IDB, or $\small D_I$.

A Datalog **Program** $\small P$ is a tuple comprising the extensional database $\small D_{E}$, the
intensional database $\small D_{I}$, and a set of queries.

$$\tag{xv}\small P=\( D_E, D_I, Q \)$$

This implies, at least, that the set of predicates accessible to queries in the program is the union
of predicates in the extensional and intensional databases.

$$\tag{xvi}\small \mathcal{P}_P = \mathcal{P}_E \cup \mathcal{P}_I$$

It should be obvious that the same exists for constants and variables;
$\small \mathcal{C}_P = \mathcal{C}_E \cup \mathcal{C}_I$ and
$\small \mathcal{V}_P = \mathcal{V}_E \cup \mathcal{V}_I$.

Datalog does not, in general, allow the rules comprising the intensional database to infer new
values for predicates that exist in the extensional database. This may be expressed as follows:

$$\tag{xvii}\small \mathcal{P}_E \cap \mathcal{P}_I = \empty$$

The same restriction is not required for constants in $\small \mathcal{C}_P$ or variables in
$\small \mathcal{V}_P$ which should be shared.

## Concrete Syntax

The definitions below uses both Extended Backus-Naur Form (EBNF) and syntax diagrams to focus on the
concrete syntax as expressed in the text representation. The EBNF definition is somewhat simplified
from the grammar used in the ASDI parser although any deviations do not significantly affect the
meaning of the language.

For the original description of the EBNF notation as it is used here, please refer to "[A.1.1
Notation](http://www.w3.org/TR/2010/REC-xquery-20101214/#EBNFNotation)" in the [XQuery
recommendation](http://www.w3.org/TR/2010/REC-xquery-20101214/). The diagrams included below were
generated by the [bottlecaps](https://www.bottlecaps.de/rr/ui) online tool from the file
[datalog.ebnf](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/doc-src/datalog.ebnf).

### Programs

A program consists of a set of facts that comprise the extensional database, a list of rules that
comprise the intensional database, and possibly a set of queries to interrogate the result of any
reasoning performed over the program.

![program](../../../doc-src/images/program.png)

```ebnf
program
        ::= pragma* ( fact | rule | query )*
```

A program consists of a single file containing facts, rules, and queries as well as any additional
files referenced via _pragmas_.

### Facts

Facts **must** be expressed in the form of ground atoms and so they have a specific rule rather
than a constrained form of the `atom` rule.

![fact](../../../doc-src/images/fact.png)

```ebnf
fact
        ::= predicate ( "(" constant ( "," constant )* ")" )? "."
```

A predicate is the identifier shared by a fact and relation.

![predicate](../../../doc-src/images/predicate.png)

```ebnf
predicate
        ::= LC_ALPHA ( ALPHA | DIGIT | "_" )*
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

![constant](../../../doc-src/images/constant.png)

```ebnf
constant
        ::= string | integer | boolean
```

Strings are described in both the identifier and quoted form in the `string` rule.

![string](../../../doc-src/images/string.png)

```ebnf
string
        ::= predicate ( ":" ALPHA ( ALPHA | DIGIT | "_" * )? )
            | DQUOTE [^\"]* DQUOTE
```

Integers bounds are currently unspecified, just strings of decimal digits.

![integer](../../../doc-src/images/integer.png)

```ebnf
integer
        ::= DIGIT+
```

Boolean values may also be represented using `⊤` (down tack `\u{22a4}`) for true, and `⊥` (up tack
`\u{22a5}`) for false where this may improve readability.

![boolean](../../../doc-src/images/boolean.png)

```ebnf
boolean
        ::= ( "true" | "⊤" ) | ( "false" | "⊥" )
```

### Rules

As facts are syntactically distinct from rules in the text representation there is no need for empty
bodies -- all rules **must** have at least one literal. Material implication may be written using
the Unicode character `⟵` (long leftwards arrow`\u{27f5}`).

![rule](../../../doc-src/images/rule.png)

```ebnf
rule
        ::= ( head | "⊥" )? ( ":-" | "<-" | "⟵" ) body "."
```

The head of a rule is a disjunction of atoms, or in the case of a constraint the head may is
optional or replaced by the value `"⊥"`.

![head](../../../doc-src/images/head.png)

```ebnf
head
        ::= ( atom ( ( ";" | "|" | "OR" | "∨" ) atom )* )
```

The body of a rule is comprised of one, or more, literals.

![body](../../../doc-src/images/body.png)

```ebnf
body
        ::= literal ( ( "," | "&" | "AND" | "∧" ) literal )*
```

The following sets of rules are equivalent.

```datalog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) <- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Y).

movie_star(X) :- star(X)  ,  movie_cast_member(X, _, _).
movie_star(X) :- star(X)  &  movie_cast_member(X, _, _).
movie_star(X) :- star(X) AND movie_cast_member(X, _, _).
movie_star(X) :- star(X)  ∧  movie_cast_member(X, _, _).
```

As described in the abstract syntax it is an error to use an extensional relation in the head of
a rule. The following will generate an error:

```datalog
parent("Xerces", brooke).

parent(X,Y) :- father(X,Y).
```

The language feature `disjunction` corresponds to the language $\small\text{Datalog}^{\lor}$ and
allows multiple atoms to appear in the rule's head with the semantics that these are choices. This
syntax will not be accepted unless the feature is enabled.

For example, the following describes the rule that _if X is a parent then X is **either** a
father **or** mother_.

```datalog
.feature(disjunction).

father(X) ;  mother(X) :- parent(X).
father(X) |  mother(X) :- parent(X).
father(X) OR mother(X) :- parent(X).
father(X) ⋁  mother(X) :- parent(X).
```

As the use of disjunction in this position in the head is _inclusive_ it is considered that any rule as above can be transformed
into the following standard form. Clearly, in this case this is not the expected semantics which would require
an exclusive disjunction, the language $\small\text{Datalog}^{\oplus}$. Because the semantics may
cause such confusion ASDI does not do this transformation by default.

```datalog
father(X) :- parent(X).
mother(X) :- parent(X).
```

The language feature `constraints` corresponds to the language $\small\text{Datalog}^{\Leftarrow}$ and
allows the specification of rules with no head. In this case the material implication symbol is
**required**, the falsum value is optional for readability, therefore the following rules are
equivalent.

```datalog
.feature(constraints).

:- alive(X) AND dead(X).
⊥ ⟵ alive(X) ∧ dead(X).
```

ASDI will disallow the addition of rules that are unsafe according to the abstract syntax. The
following are examples of unsafe rules:

* `a(X) :- b(Y).` -- because `X` appears as a distinguished variable but does not appear in a
  positive relational literal, error
  [`HeadVariablesMissingInBody`](error/enum.Error.html#variant.NegativeVariablesNotAlsoPositive).
* `a(X) :- b(Y), NOT b(X).` -- because `X` appears in a negated literal but does not appear in a
  positive relational literal, error
  [`NegativeVariablesNotAlsoPositive`](error/enum.Error.html#variant.NegativeVariablesNotAlsoPositive).
* `a(X) :- b(Y), X < Y.` -- Because `X` appears in an arithmetic literal but does not appear in a
  positive relational literal, error
  [`ArithmeticVariablesNotAlsoPositive`](error/enum.Error.html#variant.ArithmeticVariablesNotAlsoPositive).

### Atoms

The text representation of an atom is a relatively simple translation from the abstract syntax
above.

![atom](../../../doc-src/images/atom.png)

```ebnf
atom
        ::= predicate "(" term ( "," term )* ")"
```

![term](../../../doc-src/images/term.png)

```ebnf
term
        ::= variable | constant
```

Note that we explicitly separate variables into named and anonymous forms here.

![variable](../../../doc-src/images/variable.png)

```ebnf
variable
        ::= named-variable | anon-variable
```

![named-variable](../../../doc-src/images/named-variable.png)

```ebnf
named-variable
        ::= UC_ALPHA ( ALPHA | DIGIT | "_" )*
anon-variable
        ::= "_"
```

The following are all valid body atoms.

```datalog
dead(julius_caesar).
emperor(julius_caesar, rome).
emperor(X, Y).
emperor(X, rome).
```

### Literals

Any valid atom is also a valid _positive relational_ literal. The syntax below also allows for _negative_
literals as well as arithmetic expressions as literals. Conjunction may be written with the Unicode
character `∧` (logical and `\u{2227}`).

![literal](../../../doc-src/images/literal.png)

```ebnf
literal
        ::= ( "!" | "NOT" | "￢" )? ( atom | comparison )
```

The following rules are all equivalent.

```datalog
ancestor(X, Y) ⟵ parent(X, Z)  ,  ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z)  &  ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z)  ∧  ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) AND ancestor(Z, Y).
```

The language feature `negation` corresponds to the language $\small\text{Datalog}^{\lnot}$ and
allows the specification of negated literals. Negation may also be written using the Unicode
character `￢` (full-width not sign `\u{ffe2}`). The following rules are equivalent.

```datalog
.feature(negation).

alive(X) :- person(X), NOT dead(X).
alive(X) ⟵ person(X) ∧ ￢dead(X).
```

The following will fail as the negated rule is not considered safe ([Error::NegativeVariablesNotAlsoPositive]).

```datalog
.feature(negation).

alive(X) :- NOT dead(X).
alive(X) ⟵ ￢dead(X).
```

### Arithmetic Literals

The language feature `comparisons` corresponds to the language $\small\text{Datalog}^{\theta}$ and
allows the use of arithmetic literals. Comparisons take place between two literals and are
currently limited to a set of common operators. Note the addition of a string match operator, this
is similar to the Perl `=~` and requires a string value/variable on the left and a string value or
variable on the right that compiles to a valid Rust regular expression. Finally, the rule `named-term`
disallows the use of anonymous variables in arithmetic literals.

![comparison](../../../doc-src/images/comparison.png)

```ebnf
comparison
        ::= ( named-variable | constant ) operator ( named-variable | constant )
```

![operator](../../../doc-src/images/operator.png)

```ebnf
operator
        ::= "="
            | ("!=" | "/=" | "≠")
            | "<"
            | ("<=" | "≤")
            | ">"
            | (">=" | "≥")
            | ("*=" | "≛" | "MATCHES")
```

The Unicode characters `≠` (not equal to `\u{2260}`), `≤` (less-than or equal to `\u{2264}`),
`≥` (greater-than or equal to `\u{2265}`, and star equals `\u{e2899b}`) may be substituted for the
common arithmetic and string operators.

All arithmetic operations **must** be between terms of the some type, such that the property
_compatible_ introduce above is defined as:

$$\tag{xvi}\small compatible(\tau_{lhs}, \tau_{rhs}, \theta) \leftarrow \tau_{lhs} = \tau_{rhs}$$

Additionally, some operators are not present for all types, as shown in the table below.

| Type     | `=`, `≠`   | `<`, `≤`, `>`, `≥` | `≛` |
| -------- | ---------- | ------------------ | --- |
| String   | Yes        | Yes - lexical      | Yes |
| Integer  | Yes        | Yes                | No  |
| Boolean  | Yes        | No                 | No  |

The following is an example using arithmetic literals and the _car_ relation.

```datalog
.feature(comparisons).
.assert car(make: string, model: string, age: integer).

antique(X, Y) :- car(X, Y, _) AND X *= "[dD]uesenberg".
antique(X, Y) :- car(X, Y, _) AND Y = "model t".
antique(X, Y) :- car(X, Y, Z) AND Z > 50.
```

### Queries

A query is simply an atom, but one identified to the system as a goal with either the prefix `?-`
or the suffix `?`.

![query](../../../doc-src/images/query.png)

```ebnf
query
        ::= ( "?-" atom "." ) | ( atom "?" )
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
.assert car(make: string, model: string, age: integer).

car("ford", X, _)?
```

The results of this query would not include the age column:

```text
+------------+
| model      |
+============+
| edge       |
+------------+
| escort     |
+------------+
| fiesta     |
+------------+
| focus      |
+------------+
| fusion     |
+------------+
| mustang    |
+------------+
     ...
```

### Pragmas

Pragmas are declarative statements meant for the parser and runtime tooling, they do not affect
the meaning of the program itself.

![pragma](../../../doc-src/images/pragma.png)

```ebnf
pragma
        ::= "." ( feature | assert | infer | input | output )
```

The `feature` pragma determines which Datalog language is in use. Use of syntax not supported by the
selected language feature will result in errors.

![feature](../../../doc-src/images/feature.png)

```ebnf
feature
        ::= "feature" "(" feature-id ( "," feature-id )* ")" "."
```

![feature-id](../../../doc-src/images/feature-id.png)

```ebnf
feature-id
        ::= "comparisons" | "constraints" | "disjunction" | "negation"
```

```datalog
.feature(negation).
.feature(comparisons, disjunction).
```

The `assert` pragma describes a new relation in the extensional database. The parser can determine
the schema for facts from their types in the database. The use of this pragma is therefore optional,
but recommended.

![assert](../../../doc-src/images/assert.png)

```ebnf
assert
        ::= "assert" predicate "(" attribute-decl ( "," attribute-decl )* ")" "."
```

![attribute-decl](../../../doc-src/images/attribute-decl.png)

```ebnf
attribute-decl
        ::= ( predicate ":" )? ( "boolean" | "integer" | "string" )
```

```datalog
.assert human(name: string).
```

The `infer` pragma describes a new relation in the intensional database. Typically the parser
can determine the schema for relational literals from their context, The use of this pragma
is therefore optional, but recommended. The alternate form is more explicit in that it defines
an intensional relation in terms of a previously defined extensional relation.

![infer](../../../doc-src/images/infer.png)

```ebnf
infer
        ::= "infer"
            ( predicate "(" attribute-decl ( "," attribute-decl )* ")" )
            | "from" predicate "."
```

```datalog
.infer mortal(name: string).
```

Alternatively the short-cut form is often more convenient.

```datalog
.assert human(name: string).
.infer mortal from human.
```

The `input` pragma instructs the parser to load facts for the named extensional relation from an
external file. This pragma **requires** that the relation be previously defined via the `assert`
pragma.

![input](../../../doc-src/images/input.png)

```ebnf
input
        ::= "input" io-details "."
```

![io-details](../../../doc-src/images/io-details.png)

```ebnf
io-details
        ::= "(" predicate "," quoted-string ( "," quoted-string )? ")"
```

```datalog
.assert human(name: string).
.input(human, "data/humans.csv", "csv").
```

The `output` pragma instructs the parser to write facts from the named intensional relation to an
external file. This pragma **requires** that the relation be previously defined via the `infer`
pragma.

![output](../../../doc-src/images/output.png)

```ebnf
output
        ::= "output" io-details "."
```

```datalog
.infer mortal(name: string).
.output(mortal, "data/mortals.txt").
```

### Comments

Comments in Datalog are either 1) the `%` character and continue to the end of the line, or
2) C-style with `/*` to start and `*/` to end.

![comment](../../../doc-src/images/comment.png)

```ebnf
comment
        ::= "%" [^\r\n]* EOL
            | "/" "*" ( [^\*] | "*"+ [^\*\/] )* "*"+ "/"
```

```datalog
% Here's a comment
?- ancestor(xerces, X). % and another
?- ancestor(brooke /* and one inline */, X). % and another
```

# Example

The following program is the classical syllogism example, in the text representation.

```datalog
human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
```

Note in this example we allow the parser to identify the schema for the relations `human` and
`mortal` rather than using the pragmas `assert` and `infer`.

The following is the same example constructed via the ASDI library.

```rust
use asdi::{NameReferenceSet, Program};
use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Term, Variable};
use std::str::FromStr;

let mut syllogism = Program::default();

let predicates = syllogism.predicates();
let p_human = predicates.fetch("human").unwrap();
let p_mortal = predicates.fetch("mortal").unwrap();

let human = syllogism
    .add_new_extensional_relation(p_human.clone(), vec![Attribute::string()])
    .unwrap();
human.add_as_fact(["Socrates".into()]).unwrap();

let variables = syllogism.variables();
let var_x: Term = variables.fetch("X").unwrap().into();

syllogism
    .add_new_pure_rule(
        p_mortal.clone(),
        [var_x.clone()],
        [Atom::new(p_human, [var_x]).into()],
    )
    .unwrap();

syllogism
    .add_new_query(p_mortal, ["Socrates".into()])
    .unwrap();
```

The execution of this program will start with the goal query "_is Socrates mortal?_" and in
doing so will evaluate the necessary rule and derive the relation _mortal_. The result is a
boolean value denoting whether the goal is satisfied.

```text
+------------+
| _: boolean |
+============+
| true      |
+------------+
```

However, if we were to change the final query to replace the constant with a variable, as follows.

```datalog
?- mortal(X).
```

The program will select all matching (in this case all) facts from the _mortal_ relation.

```text
+------------+
| X: string  |
+============+
| "Socrates" |
+------------+
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

use crate::edb::{Attribute, Predicate, PredicateRef, Relation, RelationSet, Schema};
use crate::error::{
    extensional_predicate_in_rule_head, language_feature_unsupported, relation_does_not_exist,
    Result,
};
use crate::features::{FeatureSet, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION, FEATURE_NEGATION};
use crate::idb::eval::{Evaluator, PrecedenceGraph};
use crate::idb::query::{Query, Queryable, View};
use crate::idb::{Atom, Literal, Rule, RuleForm, RuleSet, Term, Variable, VariableRef};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// Core, readable, properties of a Datalog program.
pub trait ProgramCore {
    ///
    /// Returns the set of features currently supported by this program.
    ///
    fn features(&self) -> &FeatureSet;

    ///
    /// Returns the current set of extensional relations.
    ///
    fn extensional(&self) -> &RelationSet;

    ///
    /// Returns the current set of intensional relations.
    ///
    fn intensional(&self) -> &RelationSet;

    ///
    /// Return an iterator over the rules in the intensional database.
    ///
    fn rules(&self) -> &RuleSet;
}

///
/// A program consists of a set of extensional [`RelationSet`], a set of intensional
/// [`RelationSet`], a set of [`RuleSet`], and a set of [queries](Query).
///  
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program {
    from_file: Option<PathBuf>,
    features: FeatureSet,
    predicate_cache: NameReferenceSet<Predicate>,
    variable_cache: NameReferenceSet<Variable>,
    extensional: RelationSet,
    intensional: RelationSet,
    rules: RuleSet,
    queries: HashSet<Query>,
}

///
/// The predicate set $\small\mathcal{P}$ determines the labels of relations, atoms, and facts. This
/// type keeps a mapping of strings to [PredicateRef]s to reduce memory duplication.
///
/// ```rust
/// use asdi::Program;
///
/// let mut program = Program::default();
///
/// let p_human = program.predicates().fetch("human").unwrap();
/// let p_mortal = program.predicates().fetch("mortal").unwrap();
///
/// assert!(program.predicates().fetch("Not A Predicate").is_err());
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameReferenceSet<T>(RefCell<BTreeMap<String, AttributeNameRef<T>>>)
where
    T: AttributeName;

///
/// Attributes, the members of [Schema] are named using different types in [Relation]s and [View]s.
/// This trait identifies the minimum set of implementations required for an attribute name.
///
pub trait AttributeName:
    AsRef<str> + Clone + Debug + Display + FromStr + PartialEq + Eq + PartialOrd + Ord
{
    ///
    /// Return `true` if the string `s` is a valid value for the implementing type, else `false`.
    ///
    fn is_valid(s: &str) -> bool;

    ///
    /// Return the type identifier for the implementation, used in format errors.
    ///
    fn type_name() -> &'static str;
}

///
/// A reference type for attribute names.
///
#[allow(type_alias_bounds)]
pub type AttributeNameRef<T: AttributeName> = Rc<T>;

///
/// All collections of things in the library implement these basic methods.
///
pub trait Collection<T> {
    fn is_empty(&self) -> bool;

    fn len(&self) -> usize;

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ T> + '_>;

    fn contains(&self, value: &T) -> bool;
}

///
/// All mutable collections of things in the library implement these basic methods.
///
pub trait MutableCollection<T>: Collection<T> {
    fn iter_mut(&mut self) -> Box<dyn Iterator<Item = &'_ mut T> + '_>;

    fn add(&mut self, new: T) -> Result<()>;
}

///
/// All indexed collections of things in the library implement these basic methods.
///
pub trait IndexedCollection<K, V>: Collection<V> {
    fn get(&self, index: &K) -> Option<&V>;

    fn contains_index(&self, index: &K) -> bool;
}

///
/// All mutable, indexed, collections of things in the library implement these basic methods.
///
pub trait MutableIndexedCollection<K, V>: IndexedCollection<K, V> {
    fn get_mut<I: Into<K>>(&mut self, index: I) -> Option<&mut V>;

    fn insert<I: Into<K>>(&mut self, index: I, value: V) -> Result<()>;
}

///
/// Implemented by types that have, for sure, a label. This type is mutually exclusive with
/// [MaybeLabeled].
///
pub trait Labeled {
    ///
    /// Return the label associated with this value.
    ///
    fn label(&self) -> &Predicate;

    fn label_ref(&self) -> PredicateRef;
}

///
/// Implemented by types that have the notion of an anonymous value.
///
pub trait MaybeAnonymous {
    ///
    /// Construct a new anonymous instance.
    ///
    fn anonymous() -> Self
    where
        Self: Sized;

    ///
    /// Return `true` if this value is anonymous, else `false`.
    ///
    fn is_anonymous(&self) -> bool;
}

///
/// Implemented by types that may have a label; note that this infers the existence of an
/// anonymous value used when an instance has no label.
///
pub trait MaybeLabeled<T: AttributeName>: MaybeAnonymous {
    ///
    /// Returns this value's label, or `None` if anonymous.
    ///
    fn label(&self) -> Option<&AttributeNameRef<T>>;

    ///
    /// Returns `true` if this value has a label, else `false`.
    ///
    fn is_labeled(&self) -> bool {
        !self.is_anonymous()
    }
}

///
/// Implemented by elements of the IDB that need to distinguish between values containing only
/// constants and those that contain at least one variable.
///
pub trait MaybeGround {
    ///
    /// Returns `true` if this value is ground; defined as containing only constant values, else
    /// `false`.
    ///
    fn is_ground(&self) -> bool;
}

///
/// Implemented by types that need to distinguished between values that may contain negative literals.
///
pub trait MaybePositive {
    ///
    /// Returns `true` if this value is positive, else `false`.
    ///
    fn is_positive(&self) -> bool;

    ///
    /// Returns `true` if this value is negative, else `false`.
    ///
    fn is_negative(&self) -> bool {
        !self.is_positive()
    }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.features.is_default() {
            writeln!(f, "{}", self.features)?;
            writeln!(f)?;
        }

        if !self.extensional.is_empty() {
            for relation in self.extensional.iter() {
                writeln!(f, "{}", relation.to_schema_decl(true, false))?;
            }
            writeln!(f)?;
        }

        if !self.intensional.is_empty() {
            for relation in self.intensional.iter() {
                writeln!(f, "{}", relation.to_schema_decl(false, false))?;
            }
            writeln!(f)?;
        }

        for db in [&self.intensional, &self.extensional] {
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

impl MaybePositive for Program {
    fn is_positive(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_positive())
    }
}

impl ProgramCore for Program {
    fn features(&self) -> &FeatureSet {
        &self.features
    }

    fn extensional(&self) -> &RelationSet {
        &self.extensional
    }

    fn intensional(&self) -> &RelationSet {
        &self.intensional
    }

    fn rules(&self) -> &RuleSet {
        &self.rules
    }
}

impl Program {
    pub fn new_with_features(features: FeatureSet) -> Self {
        Self {
            from_file: None,
            features,
            predicate_cache: Default::default(),
            variable_cache: Default::default(),
            extensional: Default::default(),
            intensional: Default::default(),
            queries: Default::default(),
            rules: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// If this program were read from a source file, this will return the path to the file that
    /// was read, else `None`.
    ///
    pub fn source_file_path(&self) -> Option<&PathBuf> {
        self.from_file.as_ref()
    }

    pub(crate) fn set_source_file_path(&mut self, file_name: PathBuf) {
        self.from_file = Some(file_name);
    }

    // --------------------------------------------------------------------------------------------

    pub(crate) fn features_mut(&mut self) -> &mut FeatureSet {
        &mut self.features
    }

    // --------------------------------------------------------------------------------------------

    pub fn predicates(&self) -> &NameReferenceSet<Predicate> {
        &self.predicate_cache
    }

    pub fn variables(&self) -> &NameReferenceSet<Variable> {
        &self.variable_cache
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Returns the current set of extensional relations in a mutable state.
    ///
    pub fn extensional_mut(&mut self) -> &mut RelationSet {
        &mut self.extensional
    }

    ///
    /// Add a new relation to the extensional database with the given `label` and `schema`.
    ///
    pub fn add_new_extensional_relation<V: Into<Schema<Predicate>>>(
        &mut self,
        label: PredicateRef,
        schema: V,
    ) -> Result<&mut Relation> {
        let label = self.predicate_cache.canonical(label);
        self.extensional_mut()
            .add_new_relation(label, schema.into())
    }

    ///
    /// Add the provided `relation` to the extensional database.
    ///
    pub fn add_extensional_relation(&mut self, relation: Relation) {
        self.extensional_mut().add(relation)
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Returns the current set of intensional relations in a mutable state.
    ///
    pub fn intensional_mut(&mut self) -> &mut RelationSet {
        &mut self.intensional
    }

    ///
    /// Add a new relation to the intensional database with the given `label` and `schema`.
    ///
    pub fn add_new_intensional_relation<V: Into<Schema<Predicate>>>(
        &mut self,
        label: PredicateRef,
        schema: V,
    ) -> Result<&mut Relation> {
        let label = self.predicate_cache.canonical(label);
        self.intensional_mut()
            .add_new_relation(label, schema.into())
    }

    ///
    /// Add the provided `relation` to the intensional database.
    ///
    pub fn add_intensional_relation(&mut self, relation: Relation) {
        self.intensional_mut().add(relation)
    }

    // Note: there is no `rules_mut` as we cannot allow clients to add rules without going through
    // the program so that we can ensure schema updates.

    ///
    /// Add a new _pure_ rule to the intensional database with the given head label and terms as
    /// well as the list of body literals.
    ///
    pub fn add_new_pure_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &mut self,
        head_label: PredicateRef,
        head_terms: H,
        body: B,
    ) -> Result<()> {
        let head_label = self.predicate_cache.canonical(head_label);
        let rule = Rule::new_pure(Atom::new(head_label, head_terms), body);
        self.add_rule(rule)
    }

    ///
    /// Add a new _constraint_ rule to the intensional database with the given list of body literals.
    ///
    pub fn add_new_constraint_rule<B: Into<Vec<Literal>>>(&mut self, body: B) -> Result<()> {
        let rule = Rule::new_constraint(body);
        self.add_rule(rule)
    }

    ///
    /// Add a new _disjunctive_ rule to the intensional database with the given list of head atoms, as
    /// well as the list of body literals.
    ///
    pub fn add_new_disjunctive_rule<A: Into<Vec<Atom>>, B: Into<Vec<Literal>>>(
        &mut self,
        head: A,
        body: B,
    ) -> Result<()> {
        let rule = Rule::new_disjunctive(head, body);
        self.add_rule(rule)
    }

    ///
    /// Add the provided `rule` to the intensional database.
    ///
    pub fn add_rule(&mut self, rule: Rule) -> Result<()> {
        rule.safety_check(self.features())?;

        if rule.form() == RuleForm::Constraint && !self.features().supports(&FEATURE_CONSTRAINTS) {
            return Err(language_feature_unsupported(FEATURE_CONSTRAINTS));
        }

        if rule.form() == RuleForm::Disjunctive && !self.features().supports(&FEATURE_DISJUNCTION) {
            return Err(language_feature_unsupported(FEATURE_DISJUNCTION));
        }

        for atom in rule.head() {
            //
            // Update the database schema based on atoms found in the rule's head.
            //
            if self.extensional.contains(atom.label()) {
                return Err(extensional_predicate_in_rule_head(
                    atom.label_ref(),
                    atom.source_location().cloned(),
                ));
            } else if !self.intensional.contains(atom.label()) {
                let mut schema = Vec::with_capacity(atom.len());
                for term in atom.iter() {
                    match term {
                        Term::Anonymous => {}
                        Term::Variable(v) => schema.push(self.infer_attribute(v, &rule)),
                        Term::Constant(c) => schema.push(Attribute::from(c.kind())),
                    }
                }
                self.intensional_mut()
                    .add_new_relation(atom.label_ref(), schema)?;
            }
        }

        self.rules.add(rule);
        Ok(())
    }

    fn infer_attribute(&self, variable: &VariableRef, rule: &Rule) -> Attribute<Predicate> {
        let candidates: Vec<(&Predicate, usize)> = rule
            .literals()
            .filter_map(Literal::as_relational)
            .filter_map(|a| {
                a.iter()
                    .enumerate()
                    .filter_map(|(i, term)| term.as_variable().map(|var| (i, var)))
                    .find(|(_, var)| var == &variable)
                    .map(|(i, _)| (a.label(), i))
            })
            .collect();
        for (predicate, i) in candidates {
            if let Some(relation) = self.extensional().get(predicate) {
                return relation.schema().get(&(i.into())).unwrap().clone();
            }
        }
        Attribute::anonymous()
    }

    ///
    /// A recursive $\small\text{Datalog}$ program has rules that directly, or indirectly recurse.
    ///
    pub fn is_recursive(&self) -> bool {
        PrecedenceGraph::from(self).is_recursive()
    }

    ///
    /// In $\small\text{Datalog}^{\lnot}$ a program is _semi-positive_ **iff** the only literals that
    /// are negated are EDB relations.
    ///
    pub fn is_semi_positive(&self) -> bool {
        if self.features().supports(&FEATURE_NEGATION) {
            PrecedenceGraph::from(self).is_semi_positive()
        } else {
            false
        }
    }

    ///
    /// Linear $\small\text{Datalog}$ is defined where rule bodies have **at most** one IDB relation.
    ///
    /// $$\tag{i}\small linear\(r\) \coloneqq \ldots$$
    ///
    pub fn is_linear(&self) -> bool {
        self.rules().iter().all(|rule| {
            rule.literals()
                .filter_map(|literal| literal.as_relational())
                .filter(|atom| self.intensional().contains(atom.label()))
                .count()
                <= 1
        })
    }

    ///
    /// Guarded $\small\text{Datalog}$ is defined where for every rule, all the variables that occur
    /// in the rule bodies must occur together in at least one atom, called a guard atom.
    ///
    pub fn is_guarded(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_guarded())
    }

    ///
    /// Frontier-Guarded $\small\text{Datalog}$ is defined where for every rule, all the variables
    /// that are shared between the rule body and the rule head (called the frontier variables) must
    /// all occur together in a guard atom.
    ///
    pub fn is_frontier_guarded(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_frontier_guarded())
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Return an iterator over the queries in the program.
    ///
    pub fn queries(&self) -> impl Iterator<Item = &Query> {
        self.queries.iter()
    }

    ///
    /// Add a new query to the program with the given `label` and `schema`.
    ///
    pub fn add_new_query<T: Into<Vec<Term>>>(
        &mut self,
        label: PredicateRef,
        terms: T,
    ) -> Result<bool> {
        let label = self.predicate_cache.canonical(label);
        let query = Query::new(label, terms);
        self.add_query(query)
    }

    ///
    /// Add the provided `query` to the program.
    ///
    pub fn add_query(&mut self, query: Query) -> Result<bool> {
        let predicate = query.as_ref().label_ref();
        if !self.extensional().contains(&predicate) && !self.intensional().contains(&predicate) {
            Err(relation_does_not_exist(predicate))
        } else {
            Ok(self.queries.insert(query))
        }
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Load any data required from external files. For each relation any attached a [FilePragma](io/struct.FilePragma.html)
    /// is used to load data into that relation.
    ///
    /// All files will be loaded either relative to the current working directory, or if the program
    /// was loaded from a source file relative to that same source file.
    ///
    pub fn load_extensional_data(&mut self) -> Result<()> {
        for relation in self.extensional_mut().iter_mut() {
            relation.load_from_file()?;
        }
        Ok(())
    }

    ///
    /// Store any data required to external files. For each relation any attached a [FilePragma](io/struct.FilePragma.html)
    /// is used to store the relation's facts into a file.
    ///
    /// All files will be stored either relative to the current working directory, or if the program
    /// was loaded from a source file relative to that same source file.
    ///
    pub fn store_intensional_data(&mut self) -> Result<()> {
        for relation in self.intensional_mut().iter_mut() {
            relation.store_to_file()?;
        }
        Ok(())
    }

    // --------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------------

    ///
    /// Running a program performs the following steps:
    ///
    /// 1. load external files into the extensional database (if required),
    /// 2. call the `inference` method on the provided [Evaluator], resulting in a set of new
    ///    intensional relations,
    /// 3. merge these new relations into the existing intensional database,
    /// 4. store intensional database to external files,
    /// 5. for each query in the program, evaluate it against the new intensional database and
    ///    existing extensional database and display any results.
    ///
    pub fn run(&mut self, evaluator: impl Evaluator, load_extensional: bool) -> Result<()> {
        if load_extensional {
            self.load_extensional_data()?;
        }
        let new_idb = evaluator.inference(self)?;
        println!("{:?}", new_idb);
        self.intensional_mut().merge_from(new_idb)?;
        self.store_intensional_data()?;
        let results = self.eval_queries()?;
        for (query, view) in results {
            println!("{}", query);
            if let Some(view) = view {
                println!("{}", view);
            }
        }
        Ok(())
    }

    ///
    /// Evaluate a query against the program's current extensional and intensional databases.
    ///
    pub fn eval_query(&self, query: &Query) -> Result<Option<View>> {
        self.inner_eval_query(query, self.intensional())
    }

    ///
    /// Evaluate a query against the program's current extensional and intensional databases.
    ///
    pub fn eval_query_with(
        &self,
        query: &Query,
        _evaluator: impl Evaluator,
    ) -> Result<Option<View>> {
        let new_idb = _evaluator.inference(self)?;
        self.inner_eval_query(query, &new_idb)
    }

    ///
    /// Evaluate all the queries in the program against the program's current extensional and
    /// intensional databases.
    ///
    pub fn eval_queries(&self) -> Result<Vec<(&Query, Option<View>)>> {
        let results: Result<Vec<Option<View>>> = self
            .queries()
            .map(|q| self.inner_eval_query(q, self.intensional()))
            .collect();
        match results {
            Ok(results) => Ok(self.queries.iter().zip(results.into_iter()).collect()),
            Err(e) => Err(e),
        }
    }

    ///
    /// Evaluate all the queries in the program against the program's current extensional and
    /// intensional databases.
    ///
    pub fn eval_queries_with(
        &self,
        _evaluator: impl Evaluator,
    ) -> Result<Vec<(&Query, Option<View>)>> {
        let new_idb = _evaluator.inference(self)?;
        let results: Result<Vec<Option<View>>> = self
            .queries()
            .map(|q| self.inner_eval_query(q, &new_idb))
            .collect();
        match results {
            Ok(results) => Ok(self.queries.iter().zip(results.into_iter()).collect()),
            Err(e) => Err(e),
        }
    }

    fn inner_eval_query(&self, query: &Query, intensional: &RelationSet) -> Result<Option<View>> {
        let label = query.as_ref().label_ref();
        if intensional.contains(&label) {
            intensional.query(query)
        } else if self.extensional().contains(&label) {
            self.extensional().query(query)
        } else {
            Err(relation_does_not_exist(label))
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl<T> Default for NameReferenceSet<T>
where
    T: AttributeName,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> NameReferenceSet<T>
where
    T: AttributeName,
{
    ///
    /// Add the value `s` as a predicate in this set.
    ///
    /// This will fail if the value provided in `s` does not the check [Predicate::is_valid].
    ///
    pub fn add<S: AsRef<str>>(&self, s: S) -> Result<()> {
        self.fetch(s.as_ref()).map(|_| ())
    }

    ///
    /// Add all the values in `all` as predicates in this set.
    ///
    /// This will fail if any value provided in `all` does not the check [Predicate::is_valid].
    ///
    pub fn add_all<S: AsRef<str>>(&self, all: impl Iterator<Item = S>) -> Result<()> {
        for s in all {
            self.fetch(s.as_ref()).map(|_| ())?;
        }
        Ok(())
    }

    ///
    /// Returns `true` if there is a predicate in the set with the string representation `s`, else
    /// `false`.
    ///
    pub fn contains<S: AsRef<str>>(&self, s: S) -> bool {
        self.0.borrow().contains_key(s.as_ref())
    }

    ///
    /// Fetch will return a predicate from the set if one exists, else it will create a new
    /// predicate from `s` and add to the set and finally return this new value.
    ///
    /// This will fail if the value provided in `s` does not the check [Predicate::is_valid].
    ///
    pub fn fetch<S: Into<String>>(&self, s: S) -> Result<AttributeNameRef<T>> {
        let s = s.into();
        let found = { self.0.borrow().get(&s).cloned() };
        match found {
            None => {
                let predicate: AttributeNameRef<T> = T::from_str(&s)
                    .map_err(|_| error::invalid_value(T::type_name(), &s))?
                    .into();
                let _ = self.0.borrow_mut().insert(s, predicate.clone());
                Ok(predicate)
            }
            Some(p) => Ok(p),
        }
    }

    ///
    /// This will return the canonical predicate reference where canonical implies the first
    /// instance added to the set.
    ///
    /// Thus, if the predicate is in the set the existing one is returned, else the provided `p`
    /// is added to the set and returned.
    ///
    #[inline]
    pub fn canonical(&self, p: AttributeNameRef<T>) -> AttributeNameRef<T> {
        let s: &str = p.as_ref().as_ref();
        let found = { self.0.borrow().get(s).cloned() };
        match found {
            None => {
                let _ = self.0.borrow_mut().insert(p.to_string(), p.clone());
                p
            }
            Some(p) => p,
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Private Modules
// ------------------------------------------------------------------------------------------------

#[macro_use]
mod macros;

mod syntax;

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod error;

pub mod features;

pub mod edb;

pub mod idb;

pub mod io;

// ------------------------------------------------------------------------------------------------
// Feature-gated Modules
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "parser")]
pub mod parse;

#[cfg(feature = "typeset")]
pub mod typeset;
