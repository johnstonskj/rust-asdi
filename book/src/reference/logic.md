# Appendix: First-Order Logic & Horn Clauses

This section does not provide a primer on propositional logic, or first-order predicate logic. However, some background may be required and this section will briefly outline key terms, we refer the reader to <span class="bibref inline">[AbHuVi94](../reference/references.md#AbHuVi94), chapter 2</span>.

## Propositional logic

For Propositional logic we define a language comprising the following:

1. An infinite set of propositional _variables_, $\small \mathcal{V}$.
1. The _constant_ values `true` and `false`.
1. Propositional formulae $\small\varphi$, comprising variables, constants, the unary connective negation ($\small\lnot$) and the following binary connectives;
   1. disjunction ($\small\lor$),
   1. conjunction ($\small\land$), 
   1. implication ($\small\rightarrow$),
   1. equivalence ($\small\leftrightarrow$).

A _literal_ is a (sub-)formula comprising a single variable or constant that may be negated, i.e. $\small p$ or $\small \lnot p$.

A propositional formula $\small\varphi$ is in _conjunctive normal form_ (CNF) if it has the form 

$$\tag{i}\small\psi_1 \land \cdots \land \psi_n$$

where each formula $\small\psi$ is a disjunction of literals.

## First-Order Predicate logic

First-Order Predicate logic is a more general language than propositional logic allowing predicate symbols that range over n-ary relationships. A _first-order language_ $\small L$ is differentiated by the set of predicate symbols, constant values, and functions it chooses to allow. 

1. An infinite set of _variables_ $\small \mathcal{V}$.
1. A set of constant values, $\small \mathcal{C}$, usually including `true` and `false`.
1. A set of n-ary predicate symbols, $\small \mathcal{P}$.
1. A set of n-ary function symbols, $\small \mathcal{F}$.
1. _Well-formed formulae_ (WFF) comprising variables, constants, the unary connective negation ($\small\lnot$) and the following binary connectives;
   1. disjunction ($\small\lor$),
   1. conjunction ($\small\land$),
   1. implication ($\small\rightarrow$),
   1. equivalence ($\small\leftrightarrow$).
1. The univeral quantifier $\small\forall$ and the existential quantifier $\small\exists$.

An _atom_ corresponds to the a propositional variable and is either `true`, `false`, or $\small p(t_1, \cdots, t_n)$, where $\small p$ is a predicate symbol and $\small t_1, \cdots, t_n$ are terms.

A _term_ is either a variable, constant value, or function symbol.

Additionally, the language may include equality so as to allow atoms of the form $\small t_1 = t_2$. Note that <span class="bibref inline">[AbHuVi94](../reference/references.md#AbHuVi94)</span> uses $\small t_1 \approx t_2$.

## Horn Clauses

A Clause is simply a disjunctions of positive $\small a_1, \cdots, a_m$ and negative $\small b_1, \cdots, b_n$ literals and may be written in either of the following forms.

$$\tag{ii}\small a_1 \lor \cdots \lor a_m \lor \lnot b_1 \lor \cdots \lor \lnot b_n$$

$$\tag{iii}\small \forall x_1, \cdots, x_k (a_1 \lor \cdots \lor a_m \lor \lnot b_1 \lor \cdots \lor \lnot b_n)$$

Where the latter form introduces the set of variables used in the clause. This is often removed for clarity as the universal quantification is assumed for any variable present. This can also be written in _clausal form_, something that looks a lot like Datalog.

$$\tag{iv}\small a_1, \cdots, a_m \leftarrow b_1, \cdots, b_n$$

A _Horn Clause_ is a formula consisting of a disjunction of literals of which **at most one** is positive.

$$\tag{vi}\small a \lor \lnot b_1 \lor \cdots \lor \lnot b_n$$

or, in clausal form,

$$\tag{vii}\small a_1 \leftarrow b_1, \cdots, b_n$$

The following table describes additional clause forms along with their equivalent Datalog form.

| Propositional                                                                 | Form/Datalog           | Clausal                                                   |
| ----------------------------------------------------------------------------- | ---------------------- | --------------------------------------------------------- |
| $\small a_1(x,y) \lor a_2(x,y) \lor \lnot b_1(x,z) \lor \lnot b_2(z,y)$       | indefinite/disjunctive | $\small a_1(x,y), a_2(x,y) \leftarrow b_1(x,z), b_2(z,y)$ |
| $\small a(x,y) \lor \lnot b_1(x,z) \lor \lnot b_2(z,y)$                       | definite/pure[^1]      | $\small a(x,y) \leftarrow b_1(x,z), b_2(z,y)$             |
| $\small b(x,y)$                                                               | unit/fact[^2]          | $\small b(x,y) \leftarrow$                                |
| $\lnot b_1(x,z) \lor \lnot b_2(z,y)$                                          | goal/constraint        | $\small \leftarrow b_1(x,z), b_2(z,y)$                    |
| $\small false$ or $\small\bot$                                                | empty                  | $\small \square$                                          |


## Mapping from Datalog to First-Order Logic

The purpose of the descriptions above was to allow for the mapping of Datalog into a _first-order language_.

Given a Datalog program \$\small P=\( D_E, D_I, Q \)$ we can construct a new first-order language $\small L=(\mathcal{C},\mathcal{P},\mathcal{F})$ in the following manner.

1. $\mathcal{C}$ is comprised the set of all constant values in any EDB or IDB relation.
   $$\small \lbrace c | c \in \bigcup \lbrace terms\(atoms\(r\)\) | r \in extensional\(P\) \cap intensional\(P\) \rbrace \rbrace$$
1. $\mathcal{P}$ is comprised the set of labels for all EDB and IDB relations.
   $$\small \lbrace label\(r\) | r \in extensional\(P\) \cap intensional\(P\) \rbrace$$
1. In $\small\text{Datalog}$, which does not allow functions, $\mathcal{F}=\empty$. However, in $\small\text{Datalog}^{\theta}$ which allows certain operators in arithmetic literals $\mathcal{F}=\theta$.
1. For each rule $\small r \in rules\(P\)$:

**TBD**

----------

[^1]: In Datalog a pure rule must also follow the safety constraint that all variables that appear in the head **must also** appear in the body.

[^2]: In Datalog a fact **must also** be ground.
