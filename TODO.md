---
title: TODO
---


Composing rules with multiple instantiations                        {#compose}
===============================================================================

At the moment, the sort of rules I want to use are not possible. They rely on 
the idea that either *at least one* of the rules applicable to a node must 
lead to a closed subtableau, or that *all* of them do.

Remember that a greedy heuristic is used to determine *which rule* is to be 
chosen next, and *which formulas* the rule will consume. After all, we can 
treat the *order* of rule application as irrelevant in terms of computability 
(but not in terms of complexity --- see also [#rule-sorting]).

However, we cannot take this greedy approach when the *productions* of the 
rule can also take multiple possible values. Choosing a different production 
will not lead to a simple permutation of the same tableau: instantiations 
represent a set of *overlapping* rules, and making a choice will lead to a 
tableau that is genuinely different (see also [#rule-warnings]).

Therefore, we must try all choices at this level, until we find one that leads 
to closure. (If we could, we would try them all and only keep the shortest 
one, but that is likely prohibitively expensive; nevertheless, we could still 
offer an option to toggle shortest proof finding.)

To deal with this, I propose to add a `compose` key to the system. This key 
determines what to do with the different instantiations *generated* by the 
rule (it should not affect the degenerative constraints of a rule, since a 
constraint only limits the applicability of a rule but does not change the 
rule itself). We will call this handler the *compositor*, and it can take the 
following values:

- `nondeterministic`: Default. Different instantiations simply produce 
  different rules. Since the rules will overlap, this means that we may have 
  to try out different paths until we find one that leads to a closed tableau. 

- `greedy`: As it is now. When there are multiple instantiations of a rule, we 
  will assume that the first applicable one is the one we need. (This should 
  probably be the setting for PB-rules, as the order of application is 
  irrelevant — they don't consume any formulas off the branch anyway.)

This solves the first issue, where at least one of the instantiations must 
lead to a closed subtableau. For the second issue, where *all* instantiations 
must lead to a closed subtableau, we may add the following values:

- `disjunctive`: The productions of each (applicable) instantiation on a node 
  are taken together, each under its own branch. In practice, this will have 
  the effect that the rule will require closure under *each* instantiation --- 
  this as opposed to the `nondeterministic` value, where it requires closure 
  under *at least one* instantiation.

- `conjunctive`: The productions of each (applicable) instantiation on a node 
  are taken together, and appended to the branch one after the other. This is 
  a natural dual, but I don't see an immediate application.

#priority:high



Explicitness & clarification of terminology                        {#explicit}
===============================================================================

The word 'premise' is now used for the part of the rule that is assumed to be 
on the branch, and the word 'conclusion' for the part that may be added to the 
branch in that case. The 'constraints' are those parts of the rule that block 
a rule or multiply it into different possible instantiations.

In the YAML file, these components are at the `if`, `then` and `where` keys 
respectively. I think the terminology can be improved to refer more directly 
to the subject matter.

I suggest that:

- The premises are the 'consumptions'. The key is `consume`.
- The conclusions are the 'productions'. The key is `produce`.
- Constraints are 'instantiators'. Degenerative constraints are 'constrainers' 
  (ι-), whereas generative constraints are 'generators' (ι+).
  In the YAML, these instantiators are mixed into a single key. Perhaps it 
  would be clearer to seperate them into a `generate` and `constrain` keys.

The code should be updated to reflect this. The code should also be made more 
uniform and disambiguating in its use of words to describe 
'permissive'/'generative' constraints and 'prohibitive'/'degenerative' 
constraints, 'ε-rules'/'PB-rules', 'marks'/'signatures', etcetera.



Primitive formulas                                                {#primitive}
===============================================================================

There is a simplification method right now that strips the operators in 
formulas to the bare minimum necessary: just implication, `φ→ψ`. 

Rather than being hardcoded, this should be given in the JSON system 
definition. This way, it will be up to the user to decide which operators 
should be part of the system and which are merely derived.

A dedicated `newtype` would help with keeping primitive formulas and derived 
formulas seperate. This would also include checks to see that a formula is 
indeed primitive, and making sure that the rule premises and conclusions 
consist only of primitive formulas.

One idea calls for optional `preprocessor` key in the YAML that gives a 
translation table for the normalisation process. This provides flexibility and 
also makes it clear which formulas are not primitive:

    preprocessor:
        - "(¬A)• ⇒ A• → ⊥"



Closure                                                             {#closure}
===============================================================================

A system may treat negation through formula signatures (say, `T` and `F`) or 
by using the unary `¬` operator. Whichever method is chosen, the system should 
know when a formula directly conflicts with another so that it can detect 
branch closure.

The first instinct was to define 'negation marks', implying that any formula 
that bears one of these marks conflicts with the same formula that doesn't. 
This naturally also requires the notion of an initial mark: the user might 
want to have control over the negation of the initial formula.

This may not be the most intuitive solution. Is there are more general way to 
define 'closure' in the JSON? To what extent should simplifier logic be taken 
into account?



Citation file                                                      {#citation}
===============================================================================

Add a citation file as suggested in this 
[article](https://www.software.ac.uk/blog/2013-09-02-encouraging-citation-software-introducing-citation-files).



Warnings for overlapping/inexhaustive rulesets                {#rule-warnings}
===============================================================================

When designing a tableau system, there are two peculiarities that can arise 
for unconventional tableaux.

First, the rules might not be exhaustive, that is, there might not be a rule 
for all polarities and primitive connectives. This might be intended; the most 
we can do is issue a warning.

We might also have overlapping rules, that is, two rules may be applicable to 
(some) of the same formulas on the branch. Note that this is different from 
the problem that [#compose] attempts to solve, since rules may overlap without 
being an instance of the same meta-rule. It will be helpful to issue a warning 
in these cases. Moreover, we might want to change a setting to determine 
whether these cases are to be handled nondeterministically or greedily.



Distinguish schematic & literal variables                         {#variables}
===============================================================================

At the moment, there is no distinction between schematic variables and 
literals in formulas (but there is in evidence terms of justification logic). 
We simply interpret all variables in rule formulas as schematic, and all 
variables in tableau formulas as literal.

This may become an issue when a rule has a free variable that draws its values 
from subterms of the axioms, for example. If an axiom says that `φ→(ψ→φ)`, 
that is not just true for a specific variable assignment, but for all 
instances of the axioms.

Therefore, there should be a distinction between the two (where schematic 
variables are perhaps written as Greek or uppercase Latin characters, and 
variable literals als lowercase). They should also be handled differently.



Delayed instantiation                                 {#delayed-instantiation}
===============================================================================

For justification logic, we wish to be as quick as possible in finding the 
instance of `ψ→φ` that leads to closure for the F*-rule, or failure to close 
for the T*-rule.

Instead of 'brute-forcing' the correct instantiations for such composite or 
nondeterministic rules, it may be possible to improve efficiency by deferring 
the instantiation of free variables until a point at which they can be unified 
with true formulas on the branch or in the assumptions.

As an illustration, consider the following tableau, with `c:(B→(A→(B→A)))` in 
the constant specification:
    
    [F] y:B → ~x:A → ~(c*y)*x:(A→B)
    [F] x:A
    [T] y:B
    [T] (c*y):(?1→A→B)
    [T] c:(?1→?2→A→B) (at this point, we can resolve ?1=A,?2=B)
    [T] y:?2 (at this point, we can resolve ?2=B)

This might make things faster, but will probably also complicate the 
implementation and proof, so it was left out for now.



LaTeX output                                                          {#latex}
===============================================================================

Right now, resulting proof trees are only displayed on the terminal. There 
should be an option to write output to a file, in different formats — most 
importantly, LaTeX.

For the purpose of seperation of concerns, we might also choose to write all 
proofs in a uniform JSON format, and design a seperate program (`clerk`?) to 
lay out the proof trees into LaTeX, SVG, HTML, etcetera. A tableau can then 
also be visualised Fitch-style, for example (`--style fitch`); rules can be 
visualised `--review-rules`).

A call could be `judge -o json -s J0.yml -g 'a & b' | clerk`.



Rule sorting                                                        {#sorting}
===============================================================================

To reduce complexity, it is a helpful heuristic to apply α-rules before 
β-rules (and rules with many composite or nondeterministic instantiations 
next, and PB-rules after that).

It should be possible to provide a manual sorting of rules. It is also 
conceivable to have a setting to find a sensible ordering automatically.

Make `Rule`s an instance of `Ord` so that we can implement the greedy 
heuristic.

#priority:high



Premise sorting                                             {#premise-sorting}
===============================================================================

The premises of a rule should be sorted by complexity; we wish to match the 
most complex premise first, since it will place the most constraints on the 
rest of the premises. This should be an easy matter of `sort (compare \`on\` 
rank)`.

#priority:high



Testing                                                             {#testing}
===============================================================================

Unit tests are implemented sporadically, when needed, using 
[HUnit](http://hackage.haskell.org/package/HUnit). This should be done in a 
more organised fashion.

Parsing, for example, is untested yet fragile, because the order of parsing 
rules matters. Verify that parsing works without parentheses, with parenthesis 
on the top level, in the justification term, etcetera.



Sanity checks                                                        {#sanity}
===============================================================================

There are several conceivable sanity checks on the rulesets. These checks 
should not fail silently.

Suggested checks (work-in-progress):

- All variables in the productions of a rule must occur either in the 
  constraints or in the consumptions.
- In a disjunctive constraint, both branches must lead to a full instantiation 
  of variables in the productions.
- Every bound variable must be used in the consequent, otherwise they are 
  helper variables.
- Warn if a `bind` constraint is used on variables that were already bound.
- Set a `--maximum-depth n` argument to prevent the proof search going on for 
  too long...



YAML shortcuts                                                {#yamlshortcuts}
===============================================================================

When drawing formulas from a set of formulas, we can define some shortcuts. 
For example, `branch` is the union of `unprocessed` and `processed`, and 
`constant-specification` are those assumptions of the form `c:_`.



Selectively deactivate a rule                                  {#deactivation}
===============================================================================

For easier ad-hoc testing, provide a way to deactivate a rule with a simple 
boolean switch in the YAML.



Code practices: Failure                                           {#monadfail}
===============================================================================

The "fail" function of Monads is used quite often, because it's a natural way 
to model failure for the List and Maybe monads. However, it is discouraged 
because some monads throw errors on fail. Use a `MonadFail` class constraint 
instead (see `Control.Monad.Fail`)?



Extend existing systems                                          {#extensions}
===============================================================================

Since most logical systems tend to be extensions to 'basic' logical systems, 
we would like to avoid code duplication by building upon its basis, instead of 
repeating the same rules and axioms for every variant.

There are two obvious (not mutually exclusive choices) for this: 

- Allow an 'include' operator in the YAML to import other files.
- Allow additive specification of partial system definitions, as in: `judge 
  K.yml D.yml 4.yml`

As of now, the latter has my preference.



Remove monadic structure of pattern                            {#patternmonad}
===============================================================================

It is probably possible to refactor the patterning code; it's not really 
necessary to carry a state context over the Maybe monad, since the state *is* 
the result of the computation.



Precedence-awareness of prettyprinter                     {#prettyprecedences}
===============================================================================

The prettyprinter should be taught about precedences, so that parentheses can 
be left out if the corresponding flag (`--omit-parentheses`) is set.



Simplify degenerative constraints                       {#simplifyconstraints}
===============================================================================

An 'occurs' check is sometimes overkill: for example, if we simply want to 
check whether a formula is atomary, we shouldn't have to check whether the 
formula patterns with one of the formulas from the set of atomary formulas; we 
should just check if it is atomary. Build simpler facilities for this.



System analysis                                                    {#analysis}
===============================================================================

It would be helpful to *analyse* the tableau system — that is, generate a 
report on the combinatorial properties of a given system.

With this analysis, we can even give a progress bar, to give more of a feel of 
the complexity of a given formula.



Generate random formulas                                 {#formula-generation}
===============================================================================

Add a `--generate-random n` argument that tells `judge` not to read any goal 
formulas, just to generate `n` random ones. This will be useful for testing.



Tackle surplus constraint variable assignments                      {#surplus}
===============================================================================

We have seperation between generative constraints (`bind`) and degenerative 
constraints (`occurs`). The general rule is to put the constraints as much as 
possible in the degenerative camp, since every additional instantiation adds a 
lot of complexity to the tableau.

There are some things that the user can do to achieve this: for example, once 
a variable has been bound, it should no longer be treated as generative. 

However, sometimes we may need throwaway variables. For example, to bind a 
production `A` with formulas of the form `C:A`, we obtain an instantiation of 
`C` that we don't really need to remember. We could have assignments [C=a,A=c] 
and [C=b,A=c] ; our match needlessly increased the number of possible 
instantiations (the assignments are equivalent from a practical perspective), 
and added a lot of complexity.

Moreover, we can't short-circuit when we see that our target will never 
satisfy a constraint. If in the above example C occurred in the consumptions 
as 'd', we still need to fully check the complete set of possible assignments, 
since the substitutions were pre-generated.

A normalisation step to make every constraint an intersection of unions of 
primitive constraints, as it is implemented now, is not enough.

I propose to pre-generate the constraint, then take out all variables that 
don't need to be generative, and make them degenerative. The remainder is 
united and duplicates are removed. This will minimise the number of 
instantiations.

To achieve this, it is necessary that we can automatically translate those 
degenerated variables into a constraint that checks if a certain combination 
of assignments is allowed to occur.

#efficiency



Forgetful binding                                             {#forgetfulbind}
===============================================================================

Right now, because an `occurs` check does not bind variables, it 'forgets' 
them outside the context of its subconstraint. This is questionable, because 
variables may be complicately interrelated: if we have checked for the 
existence of a `A & B` where `A` is bound but `B` is free, expected behaviour 
is that subsequent mentions of `B` will nevertheless refer to the same `B`. An 
efficient implementation of this behaviour will need some thinking. The 
solution to [#surplus] will likely be relevant to the solution.

What should be done immediately, is to encourage explicitly removing the 
expectation that occurrences of such `B` will refer to the same `B` in cases 
that it won't, by using a `_` variable that ignores (forgets) what it was 
bound to. Perhaps use `_A`, `_B`, etcetera if the variable should stay the 
same within the context of a single formula.

This variable would also work in `bind` constraints, and would therefore also 
contribute to solving [#surplus].



Make unicode output optional                                        {#unicode}
===============================================================================

Make an option to output only ascii code, (and autodetect `$TERM` to see if 
there is unicode support).


Stretch goals                                                       {#support}
===============================================================================

Support for other proof systems
-------------------------------------------------------------------------------

Implement other systems (sequent, natural deduction, ...) and logics (modal, 
fragments of first-order, ...). 


Web front-end
-------------------------------------------------------------------------------

There are compilers for Haskell to JavaScript. A front-end for the web would 
be nice.
