# Abstract Syntax

This section describes the structure of $\small\text{Datalog}$ languages without reference to any concrete syntax
or serialized representation. The most common concrete syntax is derived from Prolog and will be described in detail in 
the [following section](concrete.md). In this section, text in **bold** indicates a key concept in the language while
text in _italics_ indicates a forward reference to such a concept.


## Programs

A Datalog **Program** $\small P$ is a tuple comprising the **Extensional** database, EDB, or  $\small D_{E}$, the
**Intensional** database, IDB, or  $\small D_{I}$, and a set of queries $\small Q$.

$$\tag{0}\small P=\( D_E, D_I, Q \)$$

The extensional database in turn is a set of _relations_ each of which is a set of _facts_ (_ground atoms_). The intensional database is a set of _rules_ that derive additional facts into intensional _relations_ via entailment. 

A program has the following properties.

* $\small program\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a program.
* $\small extensional\(P\) \equiv D_E$ returns the set of relations comprising the extensional database.
* $\small intensional\(P\) \equiv \lbrace r | r \in D_I \land relation(r) \rbrace$ returns the set of relations comprising the intensional database, **IFF** entailment has occurred. 
* $\small rules\(P\) \equiv \lbrace r | r \in D_I \land rule(r) \rbrace$ returns the set of rules associated with the intensional database.
* $\small positive(r)$ a boolean predicate which returns true **IFF** all intensional rules are _positive_:
   $$\tag{v}\small positive(p) \coloneqq \(\forall{r}\in rules\(p\); positive(r\)\)$$

## Rules

Rules $\small R$ are built from a language $\small \mathcal{L}=\( \mathcal{C},\mathcal{P},\mathcal{V}\)$
that contains the

* $\small \mathcal{C}$ — the finite sets of symbols for all constant values; e.g. `hello`, `"hi"`
   `123`,
* $\small \mathcal{P}$ — the finite set of alphanumeric character strings that begin with a
   lowercase character; e.g. `human`, `size`, `a`,
* $\small \mathcal{V}$ — the finite set of alphanumeric character strings that begin with an
   uppercase character; e.g. `X`, `A`, `Var`.

While it would appear that the values from $\small \mathcal{C}$ and $\small \mathcal{P}$ would
intersect, these values must remain distinct. For example, the value `human` is a valid predicate and
string constant, but they have distinct types in ASDI that ensure they have distinct identities.

Each rule $\small r \in R$ has the form:

$$\tag{i}\small A_1, \ldots, A_m \leftarrow L_1, \ldots, L_n$$

as well as the following properties:

* $\small rule\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a rule.
* $\small head(r)$ (the consequence), returns the set of _atom_ values $\small A_1, \ldots, A_m$ where $\small m \in \mathbb{N}$,
* $\small body(r)$ (the antecedence), returns the set of _literal_ values $\small L_1, \ldots, L_n$ where $\small n \in \mathbb{N}$,
* $\small distinguished(r)$ returns the set of _terms_ in the head of a rule,
   $$\tag{ii}\small distinguished(r) \coloneqq \lbrace t | t \in \bigcup\lbrace terms(a) | a \in head(r) \rbrace \rbrace$$
* $\small non\text{-}distinguished(r)$ returns the set of _terms_ in the body that of a rule that are not in the head,
   $$\tag{iii}\small non\text{-}distinguished(r) \coloneqq \lbrace t | t \in \( \bigcup\lbrace terms(a) | a \in body(r) \rbrace - distinguished(r) \rbrace\)\rbrace$$
* $\small ground(r)$  a boolean predicate which returns `true` **IFF** its head and its body are both _ground_:
   $$\tag{iv}\small ground\(r\) \coloneqq \(\forall{a}\in head\(r\); ground\(a\)\) \land \(\forall{l}\in body\(r\); ground\(l\)\)$$
* $\small positive(r)$ a boolean predicate which returns `true` **IFF** all body _literals_ are _positive_:
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

Datalog does not allow rules to infer new values for relations that exist in the extensional database. This may be
expressed as follows:

$$\tag{xvii}\small \mathcal{P}_E \cap \mathcal{P}_I = \empty$$

The same restriction is not required for constants in $\small \mathcal{C}_P$ or variables in
$\small \mathcal{V}_P$ which should be shared.

## Terms

Terms, mentioned above, may be constant values or variables such that
$\small\mathcal{T}=\mathcal{C}\cup\mathcal{V}\cup\bar{t}$ where $\small\bar{t}$ represents an
anonymous variable.

Terms have the following properties:

* $\small term\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a term.
* $\small constant\(t\)$ a boolean predicate which returns `true` **IFF** the term argument is a constant value.
* $\small variable\(t\)$ a boolean predicate which returns `true` **IFF** the term argument is a variable.
* $\small anonymous\(t\)$ a boolean predicate which returns `true` **IFF** the term argument is the anonymous variable, $\small\bar{t}$.

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

## Atoms

Atoms are comprised of a label, $\small p \in \mathcal{P}$, and a tuple of _terms_. A set of atoms
form a **Relation** if each _conforms to_ the schema of the relation. The form of an
individual atom is as follows:

$$\tag{ix}\small p\(t_1, \ldots, t_k\)$$

as well as the following properties:

* $\small atom\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a atom.
* $\small label\(a\)$ returns the predicate $\small p$,
* $\small terms\(a\)$ returns the tuple of term values $\small t_1, \ldots, t_k$; where
  $\small t \in \mathcal{T}$ and $\small k \in \mathbb{N}^{+}$,
* $\small arity\(a\)$ returns the cardinality of the relation identified by the predicate;
  $\small arity\(a\) \equiv |terms(a)| \equiv k$,
* in $\small\text{Datalog}^{\Gamma}$:
  * there exists a type environment $\small \Gamma$ consisting of one or more types $\small \tau$,
  * each term $\small t_i$ has a corresponding type  $\small \tau_i$ where $\small \tau \in \Gamma$,
  * $\small type\(t\)$ returns the type $\small \tau$ for that term,
  * $\small types\(a\)$ returns a tuple such that;
    $\small \(i \in \{1, \ldots, arity(a)\} | type(t_i)\)$,
* $\small ground(a)$ a boolean predicate which returns `true` **IFF** its terms are all constants:
  $$\tag{x}\small ground\(a\) \coloneqq \(\forall{t}\in terms\(a\); t \in \mathcal{C}\)$$

## Relations

Every relation $\small r$ has a schema that describes a set of attributes
$\small \lbrace \alpha_1, \ldots, \alpha_j \rbrace$, and each attribute may be labeled, and may in
$\small\text{Datalog}^{\Gamma}$ also have a type. In general, we refer to attributes by index, a value in
$\small 1, \cdots, j$.

Relations have the following properties:

* $\small relation\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a relation.
* $\small label\(r\)$ returns the predicate $\small p$,
* $\small schema\(r\)$ returns the set of attributes $\small \lbrace \alpha_1, \ldots, \alpha_j \rbrace$;
   where $\small k \in \mathbb{N}^{+}$,
* $\small arity\(r\)$ returns the number of attributes in the relation's schema, and therefore all
   atoms within the relation; $\small arity\(r\) \equiv |schema(a)| \equiv j$.
* $\small atoms\(r\)$ returns the set of atoms that comprise this relation.

> You will sometimes find the term _sort_ used instead of _schema_. This term is used with meaning derived from 
> [Model Theory](https://en.wikipedia.org/wiki/Model_theory) (see also 
> <span class="bibref inline">[Wilfred22](../reference/references.md#Wilfred22)</span>) as a partition of objects 
> into similarly structured forms. Sort is used in this way extensively in 
> <span class="bibref inline">[AbHuVi94](../reference/references.md#AbHuVi94)</span>.

Attributes have the following properties:

* $\small index\(\alpha\)$ returns the index of this attribute in the schema, where $\small index \in \lbrace 1, \cdots, j \rbrace$.
* $\small label\(\alpha\)$ returns either the predicate label of the attribute, or $\small\bot$.
* in $\small\text{Datalog}^{\Gamma}$:
  * $\small type\(\alpha\)$ returns a type $\small \tau$ for the attribute, where $\small \tau \in \Gamma$, or $\small\bot$.

The following defines a binary function that determines whether an atom $\small a$ conforms to the
schema of a relationship $\small r$.

$$\tag{xi}\small
\begin{alignat*}{2}
conforms\(a, r\) &\coloneqq &&ground\(a\) \\\\
&\land &&label\(a\) = label\(r\) \\\\
&\land &&arity\(a\) = arity\(r\) \\\\
&\land &&\forall{i} \in \[1, arity\(r\)\] \medspace conforms\( a_{t_i}, r_{\alpha_i} \)
\end{alignat*}
$$
$$\tag{xii}\small
conforms\(t, \alpha\) \coloneqq
label\(t\) = label\(\alpha\) \land
\tau_{t} = \tau_{\alpha}
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

## Literals

Literals within the body of a rule, represent sub-goals that are the required to be true for the
rule's head to be considered true.

* A literal may be an atom (termed a relational literal) or, in $\small\text{Datalog}^{\theta}$, a
  conditional expression (termed an arithmetic literal),
* an arithmetic literal has the form $\small \langle t_{lhs} \theta t_{rhs} \rangle$, where
  * $\small \theta \in \lbrace =, \neq, <, \leq, >, \geq \rbrace$,
  * in $\small\text{Datalog}^{\Gamma}$ both $\small t_{lhs}$ and $\small t_{rhs}$ terms have
    corresponding types $\small \tau_{lhs}$ and $\small \tau_{rhs}$,
  * the types $\small \tau_{lhs}$ and $\small \tau_{rhs}$ **must** be _compatible_, for some
    system-dependent definition of the property $\small compatible(\tau_{lhs}, \tau_{rhs}, \theta)$,
* in $\small\text{Datalog}^{\lnot}$ a literal may be negated, appearing as $\small \lnot l$,
* and has the following properties:
  * $\small literal\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a literal.
  * $\small relational\(l\)$ a boolean predicate which returns `true` **IFF** the literal argument is a relational literal.
  * $\small arithmetic\(l\)$ a boolean predicate which returns `true` **IFF** the literal argument is a arithmetic literal.
  * $\small terms\(l\)$ returns the set of terms in a literal,
       $$\tag{xiii}\small
       terms(l) \coloneqq
       \begin{cases}
       terms(l), &\text{if } relational(l) \\\\
       \lbrace t_{lhs}, t_{rhs} \rbrace, &\text{if } arithmetic(l) \land \text{Datalog}^{\theta}
       \end{cases}$$
  * $\small ground\(l\)$ a boolean predicate which returns `true` **IFF** its terms are all constants $\small \(\forall{t}\in terms\(l\); t \in \mathcal{C}\)$,
  * $\small positive\(l\)$ in $\small\text{Datalog}^{\lnot}$ returns `false` if negated,
       otherwise it will always return `true`.


|                        | $\small relational\(l\)$ | $\small arithmetic\(l\)$ | $\small terms\(l\)$           | $\small ground\(l\)$ | $\small positive\(l\)$ | 
|------------------------|--------------------------|--------------------------|-------------------------------|----------------------|------------------------|
| $\small p(X, Y)$       | `true`                   | `false`                  | $\small \lbrace X, Y \rbrace$ | `false`              | `true`                 |
| $\small p(X, 1)$       | `true`                   | `false`                  | $\small \lbrace X, 1 \rbrace$ | `false`              | `true`                 |
| $\small p(2, 1)$       | `true`                   | `false`                  | $\small \lbrace 2, 1 \rbrace$ | `true`               | `true`                 |
| $\small X = 1$         | `false`                  | `true`                   | $\small \lbrace X, 1 \rbrace$ | `false`              | `true`                 |
| $\small \lnot p(2, 1)$ | `true`                   | `false`                  | $\small \lbrace 2, 1 \rbrace$ | `true`               | `false`                |
| $\small \lnot X = 1$   | `false`                  | `true`                   | $\small \lbrace X, 1 \rbrace$ | `false`              | `false` †              |

> † note that while $\small \lnot X = 1$ **is** a negative literal, the corresponding literal $\small X \neq 1$ **is not**.

It is sometimes useful to consider the values `⊤` (tautology) and `⊥` (absurdity) as _pseudo-literals_ given that certain 
expressions can be reduced to them. For example, the arithmetic literal $\small 1=1$ must always be `true` and can 
therefore be reduced to `⊤` (or discarded entirely); similarly the arithmetic literal $\small 1=2$ must always be 
`false` and can therefore be reduced to `⊥`. Any rule where any literal reduces to `⊥` can itself never be `true`. 
Any rule that only comprises literals that reduce to `⊤` must always be `true`.

## Facts

Any ground rule where $\small m=1$ and where $\small n=0$ is termed a **Fact** as it is true by
nature of having an empty body, or alternatively we may consider the body be comprised of the truth
value $\small\top$.

Facts have the following properties:

* $\small fact\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a fact. 
  $$\tag{xiv}\small fact(r) \coloneqq \(ground\(r\) \land form\(r\)=pure \land body\(r\)=\empty\)$$

## Queries

An atom may be also used as a **Goal** or **Query** clause in that its constant and variable terms
may be used to match facts from the known facts or those that may be inferred from the set of rules
introduced. A ground goal is simply determining that any fact exists that matches all the
constant values provided and will return true or false.
In the case that one or more variables exist a set of facts will be returned that match the
expressed constants and provide the corresponding values for the variables.

The set of relations accessible to queries in the program is the union of relations in the extensional and 
intensional databases.

$$\tag{xvi}\small \mathcal{P}_P = \mathcal{P}_E \cup \mathcal{P}_I$$

It should be noted that the same exists for constants and variables;
$\small \mathcal{C}_P = \mathcal{C}_E \cup \mathcal{C}_I$ and
$\small \mathcal{V}_P = \mathcal{V}_E \cup \mathcal{V}_I$.

Queries have the following properties:

* $\small query\(x\)$ a boolean predicate which returns `true` **IFF** $\small x$ is a query.
