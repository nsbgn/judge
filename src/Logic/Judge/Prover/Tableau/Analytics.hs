{-|
Module      : Logic.Judge.Prover.Tableau.Analytics
Description : Analysis of the complexity of the system.
Copyright   : (c) 2017, 2018 N Steenbergen
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

This module performs rudimentary analysis of the combinatorial complexity of 
given tableau systems.
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Logic.Judge.Prover.Tableau.Analytics 
    ( analysis 
    ) where

import Prelude hiding ((<$>))
import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP

import Logic.Judge.Prover.Tableau (Ref((:=)))
import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Prover.Tableau as T

-- | Produce a 'PP.Doc' indicating the number of instances of each rule of a
-- tableau system. This function is a stub.
analysis :: F.Extension ext
         => T.TableauSystem ext 
         -> F.Formula ext 
         -> PP.Doc
analysis system goal = 
    PP.string "Number of consumer rule instantiations:" <$>
    PP.indent 4 (
        PP.vsep $ map instances rulesC
    ) <$>
    PP.string "Number of ascetic rule instantiations:" <$>
    PP.indent 4 (
        PP.vsep $ map instances (maybe [] L.toList rulesA)
    )

    where 
    
    (T.TableauSettings {T.rulesC}, T.Branch {T.rulesA}) = T.initial system goal

    instances :: T.RuleInstantiated ext 
              -> PP.Doc
    instances (T.Rule {T.name, T.generator}) = 
        PP.string name <> PP.colon <+> PP.int (length generator)

