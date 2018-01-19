judge
==============================================================================

`judge` is a modular implementation of a decision procedure for classical and 
justification logics, through a tableau-based theorem prover. 



Installation
------------------------------------------------------------------------------

After cloning the repository, the recommended installation method is through 
[Stack](https://www.stackage.org/):

    stack install judge

Alternatively, `judge` can be installed through 
[Cabal](https://www.haskell.org/cabal/users-guide/).



Usage
-------------------------------------------------------------------------------

`judge` expects a logical system to be defined in the [YAML](http://yaml.org/) 
or [JSON](http://json.org/) format. This file will specify the type of proof 
system and the logical family (although at the moment, only the respective 
values `tableau` and `justification` are recognised). It also provides the 
rules of inference. See the [logic](logic) directory for examples.

If no target formula(s) are provided via `-g`, formulas are read off the 
standard input. If no output file is provided via `-o`, the result is written 
to the standard output. By default, the format is plain text; add `-f LaTeX` 
to obtain LaTeX code instead. 

For example, the following will construct proofs for [theorems](formulas.txt) 
of the logic [Jcs](logic/J.yml) (with `c:(A→B→C) ∊ CS`), and produces a PDF 
file to visualise them:

    judge logic/J.yml \
        -a "c:(A->B->A)" \
        -f LaTeX \
         < formulas.txt \
         | pdflatex



Contributing
-------------------------------------------------------------------------------

Notable missing features are detailed on the [issue 
tracker](https://github.com/slakkenhuis/judge/issues) 
([export](https://api.github.com/repos/slakkenhuis/judge/issues)).

Contributions that extend `judge` to different logical families (modal, first 
order...) or proof systems (sequent, natural deduction...) are welcomed.

