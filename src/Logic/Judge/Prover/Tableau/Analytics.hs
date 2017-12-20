-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Prover.Tableau.Analytics
Description : Performs analysis of instantiations of tableau systems.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Logic.Judge.Prover.Tableau.Analytics where

import Prelude hiding ((<$>))
import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP

import Logic.Judge.Prover.Tableau (Ref((:=)))
import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Prover.Tableau as T


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

