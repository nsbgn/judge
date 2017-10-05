{-# LANGUAGE PackageImports #-}
module JudgeTest.Tableau.Specification where

import "HUnit" Test.HUnit

import Judge.Base (parse, parser)
import qualified Judge.Formula as F
import qualified Judge.Formula.Substitution as Fσ
import qualified Judge.Tableau.Specification as TS

import JudgeTest.Auxiliary


tests = 
    [ "merge" ~: merge 
    , "interpretConstraint" ~: interpretConstraint ]



merge :: Test
merge = test
    [ empty
    , one
    , conflict
    , conflictMultiple ]

    where

    empty = 
        let expr = TS.merge ([] :: [[Fσ.Substitution F.Justification]])
            val = []
        in  "empty" ~: expr ~?= val

    one = 
        let expr = TS.merge [[sub ["A"$="a"]]]
            val = [sub ["A"$="a"]]
        in  "one" ~: expr ~?= val

    conflict = 
        let expr = TS.merge [[sub ["A"$="a"]],[sub ["A"$:"b"]]]
            val = []
        in  "conflict" ~: expr ~?= val

    conflictMultiple = 
        let expr = TS.merge [[sub ["A"$="a"],sub ["B"$="b"]], [sub ["A"$:"b"], sub ["B"$="b"]]]
            val = [sub ["A"$="a","B"$="b"], sub ["B"$="b","A"$:"b"], sub ["B"$="b"]]
        in  "conflictMultiple" ~: expr ~?= val




interpretConstraint :: Test
interpretConstraint = test 
    [ noPattern
    , pattern1
    , pattern2
    , transform
    , choose
    , merge
    ]

    where
   
    interpret = TS.interpretConstraint [F.Formula (fJL "a -> (s:b)")] [F.Formula (fJL "c")]
    interpretPattern1 = interpret . TS.Pattern (F.Ambiguous [F.Formula $ fJL "A", F.Extension $ tJL "A"])
    interpretPattern2 = interpret . TS.Pattern (F.Ambiguous [F.Formula $ fJL "A -> B"])
    
    noPattern = 
        let expr = interpret TS.Root
            val = [mempty]
        in "no-pattern" ~: expr ~?= val

    pattern1 = 
        let expr = interpretPattern1 TS.Root
            val = [sub ["A"$="a -> s:b"]]
        in "pattern1" ~: expr ~?= val

    pattern2 = 
        let expr = interpretPattern2 TS.Root
            val = [sub ["A"$="a","B"$="s:b"]]
        in "pattern2" ~: expr ~?= val


    transform = 
        let expr = interpretPattern1 $ (TS.Transform "" (>>= F.subterms) TS.Root)
            val = [sub ["A"$="a -> s:b"], sub ["A"$="a"], sub ["A"$="s:b"], sub ["A"$="b"], sub ["A"$:"s"]]
        in "pattern2" ~: expr ~?= val

    choose = 
        let expr = interpretPattern1 (TS.Choose [TS.Root, TS.Axioms])
            val = [sub ["A"$="a -> s:b"], sub ["A"$="c"]]
        in "choose" ~: expr ~?= val

    merge = 
        let expr = interpretPattern1 (TS.Merge [TS.Pattern (F.Ambiguous [F.Formula $ F.Variable "B"]) TS.Root, TS.Pattern (F.Ambiguous [F.Formula $ F.Variable "C"]) TS.Axioms])
            val = [sub ["A"$="a -> s:b","B"$="a -> s:b","C"$="c"], sub ["A"$="c","B"$="a -> s:b","C"$="c"]]
        in "merge" ~: expr ~?= val


