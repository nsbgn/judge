judge
==============================================================================

`judge` is a modular implementation of a decision procedure for classical and 
justification logics, through a tableau-based theorem prover. 



Installation
------------------------------------------------------------------------------

After cloning the repository, the recommended installation method is through 
[Stack](https://www.stackage.org/):

    stack install judge

Alternatively, `judge` may be installed through 
[Cabal](https://www.haskell.org/cabal/users-guide/), or run in the interpreter 
(see [example.sh](example.sh)).


Usage
-------------------------------------------------------------------------------

`judge` expects a logical system to be defined in the [YAML](http://yaml.org/) 
or [JSON](http://json.org/) format. This file will specify the type of proof 
system and the logical family (although at the moment, only the respective 
values `tableau` and `justification` are recognised). It also provides the 
rules of inference. See the [logic/](logic) directory for examples.

Given a logical system `J.yml` and a target formula, say, `x:(A→B) ∧ y:A → 
x·y:B`, construct the proof as follows:

    judge -g "x:(A->B) & y:A -> x*y:B" J.yml

If no target(s) are given, formulas are read off the standard input. By 
default, the result is written as plain text to the standard output. Add `-f 
LaTeX` to obtain the output in LaTeX format. 


Contributing
-------------------------------------------------------------------------------

Notable missing features are detailed on the [issue 
tracker](https://github.com/slakkenhuis/judge/issues) 
([export](https://api.github.com/repos/slakkenhuis/judge/issues)).

Contributions that extend `judge` to different logical families (modal, first 
order...) or proof systems (sequent, natural deduction...) are welcomed.

