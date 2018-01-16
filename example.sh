#!/bin/bash

stack runhaskell -- -i'src' -i'app' -i'app/autogen' app/Main.hs \
    "logic/LP.yml" \
    -a "c:(A->(B->A))" \
     < formulas.txt
