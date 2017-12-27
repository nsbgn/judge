#!/bin/bash

function printlatex {
    judge J \
        -a "c:(A->(B->A))" \
        -f LaTeX \
        -o goals.tex \
        "logic/J.yml" < formulas.txt \
    && pdflatex goals.tex
}

function printterminal {
    stack runhaskell -- -i'src' -i'app' -i'app/autogen' app/Main.hs \
        -g "x : A & y:(A->B) -> y*(x+x') : B" \
        "logic/J.yml"
}

function comparesystems {
    for LOGIC in logic/j0-new.yml logic/j0-ghari.yml; do
        echo "Now timing: $LOGIC"
        time judge "$LOGIC" < formulas.txt
    done
}

printterminal
