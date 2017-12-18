-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Analytics
Description : Performs analysis of instantiations of tableau systems.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Logic.Judge.Tableau.Analytics where

import Prelude hiding ((<$>))
import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP

import Logic.Judge.Tableau.Specification (Ref((:=)))
import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Tableau.Specification as TS
import qualified Logic.Judge.Tableau.Algorithm as TA


analysis :: F.Extension ext
         => TS.TableauSystem ext 
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
    
    (TA.TableauSettings {TA.rulesC}, TA.Branch {TA.rulesA}) = TA.initial system goal

    instances :: TS.RuleInstantiated ext 
              -> PP.Doc
    instances (TS.Rule {TS.name, TS.generator}) = 
        PP.string name <> PP.colon <+> PP.int (length generator)

