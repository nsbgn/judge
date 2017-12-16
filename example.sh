#!/bin/bash

echo '
P & ~P 
P | ~P
x : A -> c*x : (B -> A)
x : A -> x*c : (B -> A)
x : A -> c*(x+x1) : (B->A)
' |\
runhaskell -i'src' app/Main.hs \
    -a 'c:(A->(B->A))'\
    -f LaTeX \
    -o tests.tex \
    'logic/J0-proposal.yml'

xelatex tests.tex
