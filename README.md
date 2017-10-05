judge
==============================================================================

`judge` is an implementation of a decision procedure for justification logic 
using a tableau-based theorem prover.


Installation
------------------------------------------------------------------------------

To install, do:

    stack build
    stack install judge



Example:

    judge -a 'c:(A->(B->A))' -g 'x:A->c*x:(B->A)' logics/J0.yml




