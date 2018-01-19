{-|
Module      : Logic.Judge.Formula
Description : Re-export of datastructure and class instances.
Copyright   : (c) 2017, 2018 N Steenbergen
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

Re-export of the 'Formula' datastructure, including class instances.
-}

module Logic.Judge.Formula
    ( module Logic.Judge.Formula.Datastructure
    , module Logic.Judge.Formula.Parser
    , Extension
    ) where

import Logic.Judge.Formula.Datastructure
import Logic.Judge.Formula.Parser
import Logic.Judge.Formula.Substitution


-- | Any extension of logical formulas is parseable, its extension
-- terms are subterms of its formulas, and they can be substituted into.
--
-- This encompassing class shortens class constraints and also avoids 
-- UndecidableInstances in some cases.
class (Eq e, Ord e, Parseable e, Subterm e e, HasVariables e, Substitutable e e) => Extension e
instance Extension Justification
