{-# LANGUAGE PackageImports #-}
module JudgeTest.Auxiliary where

import "base" Data.List (delete)
import "HUnit" Test.HUnit
import "text" Data.Text (pack)
import qualified "containers" Data.Map as M
import qualified "containers" Data.Tree as R

import Logic.Judge.Parser (parse, parser)
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Formula.Substitution as Fσ
import qualified Logic.Judge.Tableau.Algorithm as TA


-- * Representing formulas
fJL :: String -> F.FormulaJL
fJL = maybe (error "Invalid test formula") id . parse parser . pack

mJL :: String -> F.Marked F.FormulaJL
mJL = maybe (error "Invalid test formula") id . parse parser . pack

tJL :: String -> F.Justification
tJL = maybe (error "Invalid test formula") id . parse parser . pack


-- * For substitution construction
sub :: [(String, F.Term F.Justification)] -> Fσ.Substitution F.Justification
sub = M.fromList

-- | Represent a binding of variable to justification formula.
($:) :: String -> String -> (String, F.Term F.Justification)
n $: x = (n, F.Extension . tJL $ x)

-- | Represent a binding of variable to justification term.
($=) :: String -> String -> (String, F.Term F.Justification)
n $= x = (n, F.Formula . fJL $ x)


-- * Other

-- | Quickly delete a formula, given by a string, from a list of formulas.
except :: [F.Marked F.FormulaJL] -> String -> [F.Marked F.FormulaJL]
formulas `except` expr = delete (mJL expr) formulas

