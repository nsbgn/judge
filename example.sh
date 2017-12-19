#!/bin/bash

function printlatex {
    judge \
        -a "c:(A->(B->A))" \
        -f LaTeX \
        -o goals.tex \
        "logic/j0-new.yml" < formulas.txt \
    && xelatex goals.tex
}

function printterminal {
    runhaskell -i'src' app/Main.hs \
        -g "x : A & y:(A->B) -> y*(x+x') : B" \
        "logic/j0-new.yml"
}

function comparesystems {
    for LOGIC in logic/j0-new.yml logic/j0-ghari.yml; do
        echo "Now timing: $LOGIC"
        time judge "$LOGIC" < formulas.txt
    done
}

printlatex
