-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Analytics
Description : Performs analysis of instantiations of tableau systems.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE NamedFieldPuns #-}
module Logic.Judge.Tableau.Analytics where

import Logic.Judge.Tableau.Specification (Ref((:=)))
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Tableau.Specification as TS
import qualified Logic.Judge.Tableau.Algorithm as TA

analyseSystem :: F.Extension ext => TS.TableauSystem ext -> F.Formula ext -> IO ()
analyseSystem system goal = do
    putStrLn "Number of rule instantiations:"
    mapM_ analyseRule $ TA.rulesC θ
    maybe (return ()) (mapM_ analyseRule) $ TA.rulesA π

    where (θ, π) = TA.initial system goal


analyseRule :: TS.RuleInstantiated ext -> IO ()
analyseRule (TS.Rule {TS.name, TS.generator}) = do
    putStr $ name ++ ": "
    putStrLn $ (show . length $ generator)
