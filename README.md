judge
==============================================================================

`judge` is an implementation of a decision procedure for classical and 
justification logic. It is based on a tableau-based theorem prover. 



Installation
------------------------------------------------------------------------------

After cloning the repository, the recommended installation method is through 
[Stack](https://www.stackage.org/):

    stack build
    stack install judge

To install with [Cabal](https://www.haskell.org/cabal/users-guide/), do:
 
    cabal sandbox init
    cabal build
    cabal install

Alternatively, `judge` may be run in the interpreter via `runhaskell -i'src' 
app/Main.hs`. 



Usage
-------------------------------------------------------------------------------

`judge` expects a logical system to be defined in the [YAML](http://yaml.org/) 
or [JSON](http://json.org/) format. This file will specify the type of proof 
system and the logical family (although at the moment, only the respective 
values `tableau` and `justification` are recognised). It also provides the 
rules of inference. See the [logic/](logic) directory for examples.

Given a logical system `J.yml` and a target formula, say, `x:(A→B) ∧ y:A → 
x·y:B`, construct the proof as follows:

    judge -g 'x:(A->B) & y:A -> x*y:B' J.yml

If no target(s) are given, formulas are read off the standard input. By 
default, the result is written as plain text to the standard output. Add `-o 
file.tex -f LaTeX` to obtain LaTeX code that can be passed to 
[XeTeX](http://xetex.sourceforge.net/). 

An example run can be found in [example.sh](example.sh).



Contributing
-------------------------------------------------------------------------------

Features that are notably missing are detailed in a [todo](TODO.md) list.

Contributions that extend `judge` to different logical families (modal, first 
order...) or proof systems (sequent, natural deduction...) are welcomed.

