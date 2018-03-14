judge
==============================================================================

`judge` is a modular implementation of a decision procedure for classical and 
justification logics, through a tableau-based theorem prover. 



Installation
------------------------------------------------------------------------------

`judge` can be installed through 
[Cabal](https://www.haskell.org/cabal/users-guide/):

    cabal sandbox init
    cabal install judge


A recommended alternative is to use [Stack](https://www.stackage.org/), for 
which you will need to clone the repository and do:

    stack install



Usage
-------------------------------------------------------------------------------

`judge` expects a logical system to be defined in the [YAML](http://yaml.org/) 
or [JSON](http://json.org/) format. This file will specify the type of proof 
system and the logical family (although at the moment, only the respective 
values `tableau` and `justification` are recognised). It also provides the 
rules of inference. See the [logic](logic) directory for example 
specifications.

If no target formula(s) are provided via `-g`, formulas are read off the 
standard input. If no output file is provided via `-o`, the result is written 
to the standard output. By default, the format is plain text; add `-f LaTeX` 
to obtain LaTeX code instead. 

For example, the following will construct proofs for [theorems](formulas.txt) 
of the logic [LP](logic/LP.yml) (with `c:(A→B→A) ∊ CS`), and produces a PDF 
file to visualise them:

    judge LP \
        -a "c:(A->B->A)" \
        -f LaTeX \
         < formulas.txt \
         | pdflatex



Contributing
-------------------------------------------------------------------------------

Notable missing features are detailed on the [issue 
tracker](https://github.com/slakkenhuis/judge/issues).

Contributions that extend `judge` to different logical families (modal, first 
order...) or proof systems (sequent, natural deduction...) are welcomed.

