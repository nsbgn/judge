-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Analytics
Description : Performs analysis of instantiations of tableau systems.
License     : GPL-3
Stability   : experimental
-}

module Logic.Judge.Tableau.Analytics where

import Logic.Judge.Tableau.Specification (Ref((:=)), Guard((:|)), BaseRule((:>)))
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Tableau.Specification as TS
import qualified Logic.Judge.Tableau.Algorithm as TA

analyseSystem :: F.Extension ext => TS.TableauSystem ext -> F.Formula ext -> IO ()
analyseSystem system goal = do
    putStrLn "Number of rule instantiations:"
    mapM_ analyseRule $ TA.rulesαβ θ
    maybe (return ()) (mapM_ analyseRule) $ TA.rulesε π

    where (θ, π) = TA.initial system goal


analyseRule :: TS.Rule ext -> IO ()
analyseRule (name := _ :> _ :| constraint) = do
    putStr $ name ++ ": "
    putStrLn $ (show . sum . fmap (length . fst) $ constraint)
