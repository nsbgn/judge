#!/bin/bash

function printlatex {
    echo "p | ~p
          x : A -> c*x : (B -> A)
          x : A -> x*c : (B -> A)
          x : A -> c*(x+x') : (B->A) " |\
    runhaskell -i'src' app/Main.hs \
        -a 'c:(A->(B->A))'\
        -f LaTeX \
        -o goals.tex \
        'logic/j0-new.yml' \
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
        time judge -g "x:A -> y:(A->B) -> y*x:B" "$LOGIC"
        time judge -g "x:A -> y:(A->B) -> (y+y')*x:B" "$LOGIC"
    done
}

printlatex
