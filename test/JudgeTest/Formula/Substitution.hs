{-# LANGUAGE PackageImports #-}
module JudgeTest.Formula.Substitution where

import "HUnit" Test.HUnit

import Logic.Judge.Parser (parse, parser)
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Formula.Substitution as Fσ

import JudgeTest.Auxiliary

tests = 
    [ "pattern" ~: pattern ]


pattern :: Test
pattern = test
    [ pattern0
    , pattern1
    ]

    where

    pattern0 =
        let expr = Fσ.pattern (F.Variable "A" :: F.FormulaJL) 
                              (F.Variable "a")
            val = Just $ sub ["A"$="a"]
        in "pattern0" ~: expr ~?= val



    pattern1 =
        let expr = Fσ.pattern (F.Implication (F.Variable "A") (F.Variable "B") :: F.FormulaJL) 
                              (F.Implication (F.Variable "a") (F.Variable "b"))
            val = Just $ sub ["A"$="a","B"$="b"]
        in "pattern1" ~: expr ~?= val
