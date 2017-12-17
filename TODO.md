---
title: TODO
---


Consolidate versions                                                  {#cabal}
===============================================================================

Consolidate the versions of dependencies in the cabal file.



Clean up files                                                         {#lint}
===============================================================================

Use `HLint` to clean up the files, remove unused modules, etcetera.

Also, the directory structure irks me. `LaTeX.hs`, for example, is in its own 
file but `Printer.hs` is distributed across the printable submodules. I think 
combining some files will make things clearer.



Clean up output                                                      {#stderr}
===============================================================================

Anything that is not exported (e.g. diagnostics) should go to stderr. Also 
make the output more informative: show the formula upon failure, and show the 
simplification step in a succesful proof.



Closure                                                             {#closure}
===============================================================================

Closure is now saved on the branch and then checked during the algorithm. This 
seems unnecessary — can it not be checked dynamically in the first place?

Secondly, a system may treat negation through formula signatures (say, `T` and 
`F`) or by using the unary `¬` operator. Whichever method is chosen, the 
system should know when a formula directly conflicts with another, so that it 
can detect branch closure.

The first instinct was to define 'negation marks', implying that any formula 
that bears one of these marks conflicts with the same formula that doesn't. 
This naturally also requires the notion of an initial mark: the user might 
want to have control over the negation of the initial formula.

This may not be the most intuitive solution. Is there are more general way to 
define 'closure' in the JSON/YAML? To what extent should simplifier logic be 
taken into account?



Premise sorting                                             {#premise-sorting}
===============================================================================

The premises of a rule should be sorted by complexity; we wish to match the 
most complex premise first, since it will place the most constraints on the 
rest of the premises. This should be an easy matter of `sort (compare \`on\` 
rank)`.

#priority:high



Composing rules with multiple instantiations                        {#compose}
===============================================================================

It is as of now possible to define rules such that *at least one* of the rules 
applicable to a node must lead to a closed subtableau, by defining multiple 
instances of a single rule, using the `compose` key. However, it is not yet 
possible to say that *all* instantiations must lead to a closed subtableau. To 
solve this, we may add the following values to the `compose` key:

- `disjunctive`: The productions of each (applicable) instantiation on a node 
  are taken together, each under its own branch. In practice, this will have 
  the effect that the rule will require closure under *each* instantiation --- 
  this as opposed to the `nondeterministic` value, where it requires closure 
  under *at least one* instantiation.

- `conjunctive`: The productions of each (applicable) instantiation on a node 
  are taken together, and appended to the branch one after the other. This is 
  a natural dual, but I don't see an immediate application.



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



JSON output                                                            {#json}
===============================================================================

For the purpose of seperation of concerns, we might also choose to write all 
proofs in a uniform JSON format, and design a seperate program (`clerk`?) to 
lay out the proof trees into LaTeX, SVG, HTML, etcetera. A tableau can then 
also be visualised Fitch-style, for example (`--style fitch`); rules can be 
visualised `--review-rules`).

A call could be `judge -o json -s J0.yml -g 'a & b' | clerk`.



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

- All variables in the productions and in the constraints of a rule must occur 
  either in the generator or in the consumptions.
- In a disjunctive constraint, both branches must lead to a full instantiation 
  of variables in the productions.
- Every bound variable must be used in the consequent, otherwise they are 
  helper variables and should be eliminated in the final assignment.
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



Autosorting                                                     {#autosorting}
===============================================================================

To reduce complexity, it is a helpful heuristic to apply α-rules before 
β-rules (and rules with many composite or nondeterministic instantiations 
next, and PB-rules after that). This can be achieved manually by giving the 
rules in the appropriate order in the specification.

It is also conceivable to have a setting to find a sensible ordering 
automatically, when `autosort:true` is specified. Make `Rule`s an instance of 
`Ord` so that we can implement the greedy heuristic.



Code practices                                                    {#practices}
===============================================================================

- The "fail" function of Monads is used quite often, because it's a natural 
  way to model failure for the List and Maybe monads. However, it is 
  discouraged because some monads throw errors on fail. Use a `MonadFail` 
  class constraint instead (see `Control.Monad.Fail`)?

- 'TableauSettings' should perhaps not be passed as a parameter. Rather, it is 
  a state in a 'Reader' monad.



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

