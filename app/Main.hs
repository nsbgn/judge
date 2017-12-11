{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

import "base" System.IO (IOMode(WriteMode))
import "base" GHC.IO.Handle (Handle)
import "base" GHC.IO.Handle.FD (stdout, openFile)
import "base" Control.Monad (forM_)
import "base" Data.Maybe (fromJust)
import "text" Data.Text (Text, pack, unpack)
import qualified "yaml" Data.Yaml as Y
import qualified "unordered-containers" Data.HashMap.Strict as M2

import Logic.Judge.Printer (prettyprint, prettyprintH)
import qualified Logic.Judge.CLI as CLI
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Tableau.Specification as TS
import qualified Logic.Judge.Tableau.Algorithm as TA
import qualified Logic.Judge.Tableau.Yaml as TY
import qualified Logic.Judge.Tableau.Printer as TP
import qualified Logic.Judge.Tableau.Analytics as TX

main :: IO ()
main = do
    arg <- CLI.arguments
    yaml <- deserialiseGeneric (CLI.infile arg)
    h <- maybe (return stdout) (flip openFile WriteMode) $ CLI.outfile arg
    case yaml .: "logic" of
        "justification" -> case yaml .: "system" of
            "tableau" -> do
                tableausystem <- addAssumptions 
                    <$> (deserialise yaml :: IO (TS.TableauSystem F.Justification))
                    <*> (CLI.assumptions arg :: IO [F.FormulaJL])
                --prettyprint tableausystem
                goals <- CLI.goals arg :: IO [F.FormulaJL]
                forM_ goals $ \goal -> do
                    prettyprintH h goal
                    if CLI.verbose arg
                        then TX.analyseSystem tableausystem goal
                        else return ()
                    prettyprintH h (TA.decide tableausystem goal)
                    ln
            value -> unknown value "system"
        value -> unknown value "logic"

    where

    ln :: IO ()
    ln = putStrLn ""

    addAssumptions :: TS.TableauSystem ext -> [F.Formula ext] -> TS.TableauSystem ext
    addAssumptions system assumptions = 
        system { TS.assumptions = assumptions ++ TS.assumptions system }

    unknown :: Text -> Text -> IO ()
    unknown value key =
        fail $ "value '" ++ unpack value ++ "' for '" ++ unpack key ++ "' is unknown."


-- | Get value from a YAML map at a key. If it doesn't exist, return @"undefined"@.
(.:) :: Y.Value -> Text -> Text
(Y.Object object) .: key = 
    let asText (Y.String s) = Just s
        asText _            = Nothing
    in maybe "undefined" id $ M2.lookup key object >>= asText 
_ .: _ = "undefined"


-- | Read a filename into a generic YAML value.
deserialiseGeneric :: String -> IO Y.Value
deserialiseGeneric path = either report return =<< Y.decodeFileEither path
    where report = fail . Y.prettyPrintParseException


-- | Evaluate generic YAML into a typed value.
deserialise :: Y.FromJSON a => Y.Value -> IO a
deserialise = either (fail . show) return . Y.parseEither Y.parseJSON 

