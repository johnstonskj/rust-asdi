# Relational Algebra Mapping


Every expression in the _basic_ relational algebra can be expressed as a $\small\text{Datalog}$
query. However, operations in the extended relational algebra (grouping, aggregation, and sorting)
have no corresponding capability in $\small\text{Datalog}$. Similarly, $\small\text{Datalog}$ can
express recursion, which the relational algebra cannot. The following describes the foundational
relational operations and their $\small\text{Datalog}$ equivalent.

The semantics of $\small\text{Datalog}$ relations are primarily set based we would expect common
set operations to work in $\small\text{Datalog}$ rules. Given the relations R, $\small r(a,b,c)$,
and S, $\small s(d,e,f)$, we can easily describe the common set operations.

## Union ($\small\cup$)

Union is a binary operator that is written as $\small R \cup S$, where
$\small R$ and $\small S$ are relations.

For set union and set difference, the two relations involved must be **union-compatible** -—
that is, the two relations must have the same set of attributes. A strict form of this rule does not
allow for same-named attributes whereas a looser form allows duplicate names IFF they have identical
types. Because set intersection is defined in terms of set union and set difference, the two
relations involved in set intersection must also be union-compatible.

Consider the expression $\small U \coloneqq R \cup S$, which can be expressed as follows:

```datalog
u(X, Y, Z) :- r(X, Y, Z).
u(X, Y, Z) :- s(X, Y, Z).
```

## Difference ($\small\setminus$)

Difference is a binary operator that is written as $\small R \setminus S$,
where $\small R$ and $\small S$ are relations. This cannot be
implemented in $\small\text{Datalog}$ as it required negation.

Consider the expression $\small D \coloneqq R \setminus S$ which can be expressed in
$\small\text{Datalog}^{\lnot}$ as follows:

```datalog
d(X,Y,Z) :- r(X,Y,Z) AND NOT s(X,Y,Z).
```

## Intersection ($\small\cap$)

Intersection is a binary operator that is written as $\small R \cap S$, where
$\small R$ and $\small S$ are relations.

Consider the expression $\small I \coloneqq R \cap S$, which can be expressed as follows:

```datalog
i(X,Y,Z) :- r(X,Y,Z) AND s(X,Y,Z).
```

Intersection may be defined in terms of the difference operation, as below.

$$\small R \cap S \enspace\equiv\medspace R \setminus (R \setminus S)$$

## Cartesian Product ($\small\times$)

Cartesian Product is a binary operator that is written as $\small R \times S$,
where $\small R$ and $\small S$ are relations. In relational algebra it is required that the two
relations involved must have disjoint headers—that is, they must not have a common attribute name.

The following is a valid expression of $\small P \coloneqq R \times S$:

```datalog
p(A, B, C, D, E, F) :- r(A, B, C) AND s(D, E, F).
```

## Projection ($\small\Pi$)

Projection is a unary operation written as
$\small\Pi_{a_{1},\ldots ,a_{n}}(R)$ where $a_{1},\ldots ,a_{n}$ is a set of attribute names. The
result of such projection is defined as the set that is obtained when all tuples in $R$ are
restricted to the set $\small\lbrace a_{1},\ldots ,a_{n}\rbrace$. In some literature the lower case
$\small\pi$ is used instead of $\small\Pi$.

For example, the projection $\small P \coloneqq \Pi_{X} \(R\)$, a projection of the first attribute in R only,
can be expressed in $\small\text{Datalog}$ as either of the following equivalent rules.

```datalog
p(X) :- r(X, Y, Z).
p(X) :- r(X, _, _).
```

## Generalized Selection ($\small\sigma$)

As defined by Codd, selection is written as $\small \sigma_{a\theta b}(R)$ or
$\small \sigma_{a\theta v}(R)$ where:

* $\small a$ and $\small b$ are attribute names,
* $\small\theta$ is a binary operation, where $\small\theta\in\lbrace =, \neq, <, \leq, >, \geq \rbrace$,
  * ASDI adds the non-standard _string match_ operator $\small\overset{\star}{=}$ to the set $\small\theta$,
* $\small v$ is a constant value,
* $\small R$ is a relation,

The selection $\small\sigma_{a\theta b}(R)$ denotes all tuples in $\small R$ for which $\small\theta$
holds between the $\small a$ and the $\small b$ attribute.

The selection $\small\sigma_{a\theta v}(R)$ denotes all tuples in $\small R$ for which $\small\theta$
holds between the $\small a$ attribute and the value $\small v$.

Selection requires arithmetic literals and therefore the languages $\small\text{Datalog}^{\theta}$
and $\small\text{Datalog}^{\lnot}$ for negation.

**Generalized Selection** is a unary operation written as $\small\sigma_{\varphi}(R)$ where
$\small\varphi$ is a propositional formula that consists of conditions as allowed in the normal selection
and the logical operators $\small\land$ (and), $\small\lor$ (or) and $\small\lnot$ (negation). This selection
selects all those tuples in $\small R$ for which $\small\varphi$ holds.

In $\small\text{Datalog}^{\theta}$ the selection $\small L \coloneqq \sigma_{X>100 \land Y=‘something’} \(R\)$
can be expressed as follows, where both rules are equivalent.

```datalog
l(X, Y, Z) :- r(X, Y, Z) AND X > 100 AND Y = something.
l(X, Y, Z) :- r(X, something, Z) AND X > 100.
```

## Rename ($\small\rho$)

Rename is a unary operation written as $\small\rho_{a/b}\(R\)$ where the result is identical to $\small R$
except that the $\small b$ attribute in all tuples is renamed to an $\small a$ attribute. This is
simply used to rename the attribute of a relation or the relation itself. There is also the
$\small\rho_{x(A_{1},\ldots, A_{n})}\(R\)$ notation, where $\small R$ is renamed to $\small x$ and the
attributes $\small\lbrace a_{1},\ldots, a_{n}\rbrace$ are renamed to $\small\lbrace A_{1}, \ldots, A_{n}\rbrace$.

TBD

## Theta Join ($\small\Join_{\theta}$)

Theta Join is a binary operator that is written as $\small{R \Join S} \atop {a \theta b}$ and
$\small{R \Join S} \atop {a \theta v}$ or alternatively as $\small R \Join_{a \theta b} S$ and
$\small R \Join_{a \theta v} S$, where $\small R$ and $\small S$ are relations,
and the expressions $\small a\theta b$ and $\small a\theta v$ should be interpreted in the same way
as for selection.

A **generalized Theta Join** can be described following the convention of generalized selection where
$\small\theta$ expands into a propositional formula in the same manner as $\small\varphi$. Unfortunately
there is no notational alignment where a generalized theta join might be signified as
$\small\Join_{\varphi}$

For example, the natural join $\small J \coloneqq R \Join_{S.X>100} S$, a join conditional join on an attribute
in S can be expressed in $\small\text{Datalog}^{\theta}$ as follows.

```datalog
j(X,Y,Z) :- r(X,Y,Z) AND s(Xs,Y,Z) AND Xs > 100.
```

It is possible to dispense with the theta join in most cases as it can be expressed in terms of
the selection and cartesian product operators described above.

$$\small R \Join_{\theta} \enspace\equiv\enspace \sigma_{\theta}\(R \times S\)$$

An **Equi-Join** is a special case of the Theta Join where $\small\theta$ contains only equalities.

## Natural Join ($\small\Join$)

Natural Join is a binary operator that is written as $\small R \Join S$, where $\small R$ and
$\small S$ are relations. The result of the natural join is the set of all combinations of tuples
in $\small R$ and $\small S$ that are equal on their common attribute names. The natural join is
arguably one of the most important operators since it is the relational counterpart of the logical
AND operator.

For example, the natural join $\small J = R \Join S$, a join on the first two attributes of R and S,
can be expressed in $\small\text{Datalog}$ as follows. Note that the common attributes are defined
by the names of their variables, not the underlying names of the attribute schema.

```datalog
j(X,Y,Z,Q) :- r(X,Y,Z) AND s(X,Y,Q).
```

A Natural Join is a special case of the Equi-Join where equality operations are performed on all
common attributes.

## Complex Expressions

More complex examples can the be made from combining relational operators. The relational
query $\small A \coloneqq \Pi_{X}\(\sigma_{Y = 3} \(R\) \)$ becomes

```datalog
a(X) :- r(X, 3, _).
```

Similarly, the relational query $\small A \coloneqq \Pi_{X}\(\sigma_{Y = 3} \(R\) \Join_{R.X=S.X}\sigma_{Y = 5} \(S\)\)$
becomes

```datalog
a(X) :- r(X, 3, _) AND s(X, 5, _).
```

## Recursion

Although relational algebra seems powerful enough for most practical purposes, there are some
simple and natural operators on relations that cannot be expressed by relational algebra.
One of them is the transitive closure of a binary relation. Given a domain $\small D$, let binary
relation $\small R$, $\small \text{Datalog } r(x, y)$, be a subset of $\small D\times D$. The transitive closure $\small R^{+}$ of $R$ is the smallest
subset of $\small D\times D$ that contains $R$ and satisfies the following condition:

$$\small\forall x\forall y\forall z\left((x,y)\in R^{+}\land (y,z)\in R^{+}\Rightarrow (x,z)\in R^{+}\right)$$

In Datalog this form of recursion is naturally expressed, as follows.

```datalog
r_plus(X, Z) :- r(X, Y), r_plus(Y, Z).
```

## Acknowledgements

Examples in this section are taken from _Introduction to Data Management CSE 344, Lecture 10:
Datalog_<span class="bibref">[Balaz12](../reference/references.md#Balaz12)</span>, and definitions for 
the relational algebra are taken from _Relational algebra_<span 
class="bibref">[WikiRelalg](../reference/references.md#WikiRelalg)</span>. The _Relational Algebra Query Converter_<span class="bibref">[QConv](../reference/references.md#QConv)</span> is a useful tool to take a SQL query 
and convert to relational algebra which can then be converted to Datalog using the examples above.
