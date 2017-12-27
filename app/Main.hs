{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

import "base" GHC.IO.Handle (Handle, hClose)
import "base" GHC.IO.Handle.FD (stdout, stderr)
import "base" Control.Monad (forM_)
import "base" Data.Maybe (fromJust)
import "text" Data.Text (Text, pack, unpack)
import qualified "yaml" Data.Yaml as Y
import qualified "unordered-containers" Data.HashMap.Strict as M2

import qualified CLI

import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Prover.Yaml as TY
import qualified Logic.Judge.Prover.Tableau as T
import qualified Logic.Judge.Prover.Tableau.Analytics as TX
import qualified Logic.Judge.Writer as W


main :: IO ()
main = do
    arg <- CLI.arguments
    yaml <- CLI.infile arg >>= deserialiseGeneric

    case yaml .: "logic" of
        "justification" -> case yaml .: "system" of
            "tableau" -> do
                sys <- addAssumptions 
                    <$> (deserialise yaml :: IO (T.TableauSystem F.Justification))
                    <*> (CLI.assumptions arg :: IO [F.FormulaJL])

                -- Tableau system is not prettyprinted well, so won't be shown
                -- even in verbose mode for now
                --if CLI.verbose arg
                --    then write stderr $ pretty sys
                --    else return ()

                targets <- CLI.goals arg :: IO [F.FormulaJL]
                file <- CLI.outfile arg
                let format = CLI.format arg 

                W.writeHeader file format
                forM_ targets $ \φ -> do

                    if CLI.verbose arg
                        then W.prettyprint stderr $ TX.analysis sys φ
                        else return ()
                    
                    W.writeBody file format (T.decide sys φ)
                W.writeFooter file format

                if file /= stdout
                    then hClose file
                    else return ()


            value -> unknown value "system"
        value -> unknown value "logic"
    

    where

    addAssumptions :: T.TableauSystem ext -> [F.Formula ext] -> T.TableauSystem ext
    addAssumptions system assumptions = 
        system { T.assumptions' = assumptions ++ T.assumptions' system }

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


