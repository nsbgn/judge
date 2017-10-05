{-# LANGUAGE PackageImports #-}
module JudgeTest.Tableau.Algorithm where

import "base" Data.List (delete)
import "HUnit" Test.HUnit
import "text" Data.Text (pack)
import qualified "containers" Data.Map as M
import qualified "containers" Data.Tree as R

import Judge.Base (parse, parser)
import qualified Judge.Formula as F
import qualified Judge.Formula.Substitution as Fσ
import qualified Judge.Tableau.Algorithm as TA

import JudgeTest.Auxiliary


{-
tests = 
    [ "matchesND" ~: matchesND
    , "constrainedBy" ~: constrainedBy ]


matchesND :: Test
matchesND = test 
    [ emptyPremise
    , onePremise
    , twoPremises
    , catchAll
    , evidenced
    ]
   
    where 

    targets = map mJL ["[T,A,B] p", "[T] r -> s", "[F] t -> u", "[T,A] p -> q", "[T] p -> z", "t:a"]

    emptyPremise =
        let expr = [] `TA.matchesND` targets
            val  = [(mempty, mempty, targets)]
        in  "empty-premise" ~: expr ~?= val

    onePremise =
        let expr = [ mJL "[T] A -> B" ] `TA.matchesND` targets
            val  = [ ([],    sub ["A"$="r", "B"$="s"], targets `except` "[T] r -> s") 
                   , (["A"], sub ["A"$="p", "B"$="q"], targets `except` "[T,A] p -> q")
                   , ([],    sub ["A"$="p", "B"$="z"], targets `except` "[T] p -> z")
                   ]
        in  "one-premise" ~: expr ~?= val

    twoPremises = 
        let expr = [ mJL "[T] A", mJL "[T] A -> B" ] `TA.matchesND` targets
            val  = [ (["A"], sub ["A"$="p", "B"$="q"], targets `except` "[T,A,B] p" `except` "[T,A] p->q")
                   , ([],    sub ["A"$="p", "B"$="z"], targets `except` "[T,A,B] p" `except` "[T] p->z")
                   ]
        in  "two-premise" ~: expr ~?= val

    catchAll = 
        let expr = [ mJL "A" ] `TA.matchesND` targets
            val  = map (\(F.Marked m x, xs) -> (m, M.fromList [("A", F.Formula x)], xs)) (TA.drawsFrom targets)
        in "catchall" ~: expr ~?= val

    evidenced = 
        let expr = [ mJL "S:A" ] `TA.matchesND` targets
            val  = [ ([], sub ["S"$:"t", "A"$="a"], targets `except` "t:a") ]
        in "evidenced" ~: expr ~?= val



constrainedBy :: Test
constrainedBy = test 
    [ noConstraints 
    , emptyConstraint
    , conflictingConstraint 
    , mergingConstraints ]
    
    where

    noConstraints = 
        let expr = mempty `TA.constrainedBy` ([] :: [Fσ.Substitution F.Justification])
            val  = [] 
        in "no-constraint" ~: expr ~?= val 

    emptyConstraint = 
        let expr = mempty `TA.constrainedBy` ([mempty] :: [Fσ.Substitution F.Justification])
            val  = [(mempty, [])] 
        in "empty-constraint" ~: expr ~?= val 
    
    conflictingConstraint = 
        let expr = sub ["A"$="a"] `TA.constrainedBy` [sub ["A"$:"a","B"$="b"]]
            val  = [] 
        in "conflicting-constraint" ~: expr ~?= val 

    mergingConstraints = 
        let expr = sub ["A"$="a"] `TA.constrainedBy` [sub ["A"$:"a"], sub ["B"$="b"], sub ["C"$="c"]]
            val  = [ (sub ["A"$="a","B"$="b"], [sub ["A"$:"a"],sub ["C"$="c"]])
                   , (sub ["A"$="a","C"$="c"], [sub ["A"$:"a"],sub ["B"$="b"]])
                   ] 
        in "merging-constraint" ~: expr ~?= val 
-}
