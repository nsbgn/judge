{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

import "base" System.IO (IOMode(WriteMode))
import "base" GHC.IO.Handle (Handle, hClose)
import "base" GHC.IO.Handle.FD (stdout, openFile)
import "base" Control.Monad (forM_)
import "base" Data.Maybe (fromJust)
import "text" Data.Text (Text, pack, unpack)
import qualified "yaml" Data.Yaml as Y
import qualified "unordered-containers" Data.HashMap.Strict as M2

import Logic.Judge.Printer (pretty, write)
import Logic.Judge.LaTeX (latex, latexHeader, latexFooter)
import qualified Logic.Judge.CLI as CLI
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Tableau.Specification as TS
import qualified Logic.Judge.Tableau.Algorithm as TA
import qualified Logic.Judge.Tableau.Yaml as TY
import qualified Logic.Judge.Tableau.Printer as TP
import qualified Logic.Judge.Tableau.Analytics as TX


header :: Handle -> CLI.Format -> IO ()
header h CLI.LaTeX = write h latexHeader
header h _ = return ()

footer :: Handle -> CLI.Format -> IO ()
footer h CLI.LaTeX = write h latexFooter
footer h _ = return ()


main :: IO ()
main = do
    arg <- CLI.arguments
    yaml <- deserialiseGeneric (CLI.infile arg)
    h <- maybe (return stdout) (flip openFile WriteMode) $ CLI.outfile arg
    header h $ CLI.format arg
    case yaml .: "logic" of
        "justification" -> case yaml .: "system" of
            "tableau" -> do
                sys <- addAssumptions 
                    <$> (deserialise yaml :: IO (TS.TableauSystem F.Justification))
                    <*> (CLI.assumptions arg :: IO [F.FormulaJL])
                φs <- CLI.goals arg :: IO [F.FormulaJL]

                if CLI.verbose arg
                    then write stdout (pretty sys)
                    else return ()

                forM_ φs $ \φ -> do

                    write stdout (pretty φ)
    
                    if CLI.verbose arg
                        then TX.analyseSystem sys φ
                        else return ()
                    
                    let θ = TA.decide sys φ
                    case CLI.format arg of
                        CLI.LaTeX -> write h $ latex θ
                        _         -> write h $ pretty θ

            value -> unknown value "system"
        value -> unknown value "logic"
    
    footer h $ CLI.format arg
    if h /= stdout
        then hClose h
        else return ()

    where

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


