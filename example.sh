#!/bin/bash

echo '
P & ~P 
P | ~P
x : A -> c*x : (B -> A)
x : A -> x*c : (B -> A)
r+s:(A->A)
' |\
runhaskell -i'src' app/Main.hs 'logic/J0-proposal.yml' -v -a 'c:(A->(B->A))'

runhaskell -i'src' app/Main.hs 'logic/J0-proposal.yml' -v -a 'd:A' -g 'c*d:(B->A)' -g 'd*c:(B->A)'
