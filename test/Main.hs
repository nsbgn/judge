import Test.HUnit

import JudgeTest.Formula.Substitution as Fσ

main = runTestTT . test $
    [ "Formula.Substitution" ~: Fσ.tests
    ]
