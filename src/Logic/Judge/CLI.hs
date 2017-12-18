-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.CLI
Description : Options to judge's command-line interface.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
module Logic.Judge.CLI where

import Prelude hiding (getContents)
import "text" Data.Text (Text, pack, unpack)
import "text" Data.Text.IO (getContents)
import "base" Data.Monoid ((<>))
import qualified "optparse-applicative" Options.Applicative as O
import qualified "attoparsec" Data.Attoparsec.Text as P
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP

import Logic.Judge.Parser (parse, parser, Parseable)

data Arguments = Arguments 
    { verbose      :: Bool
    , _assumptions :: [String]
    , _goals       :: [String]
    , outfile      :: Maybe String
    , format       :: Format
    , infile       :: String
    }


data Format 
    = LaTeX 
    | Plain 
    deriving (Show, Read)


arguments :: IO Arguments
arguments = O.execParser prog 

    where

    prog =  O.info
        (   O.helper <*> options )
        (   O.fullDesc 
            <>  O.progDescDoc 
                (Just $
                    PP.empty PP.<$$>
                    (PP.fillSep . map PP.text . words $ description) PP.<$$>
                    PP.empty PP.<$$> 
                    (PP.underline . PP.text $ "Example call:") PP.<$>
                    (PP.indent 4 $ PP.text "judge J.yml < formulas.txt")
                )
            <>  O.header "judge - Decision procedure for justification logic" 
            <>  O.footer "2017, Utrecht University"
        )

    options = Arguments
        <$> O.switch
            (   O.short 'v' <> O.long "verbose"
            <>  O.help "Show diagnostics"
            )
        <*> O.many ( O.strOption
            (   O.short 'a' <> O.long "assumption" <> O.metavar "EXPR"
            <>  O.help "Add assumption(s). May be provided multiple times"
            )
            )
        <*> O.many ( O.strOption
            (   O.short 'g' <>  O.long "goal" <> O.metavar "EXPR"
            <>  O.help "Set goal formula(s). Will be read from standard \n\
                       \input if none are given. May be provided multiple \n\
                       \times"
            )
            )
        <*> ( O.optional $ O.strOption
            (   O.short 'o' <>  O.long "output" <> O.metavar "PATH"
            <>  O.help "Output file (default: standard input)"
            )
            )
        <*> ( O.option O.auto
            (   O.short 'f' <>  O.long "format" <> O.metavar "FMT"
            <>  O.value Plain <> O.showDefault
            <>  O.help "Output format"
            )
            )
        <*> ( O.argument O.str (O.metavar "LOGIC") )

    description = "Decides whether given logical formulas are provable in \n\
                  \some logical system. Takes a YAML or JSON file as input. \n\
                  \Refer to my Master's thesis for specification."


-- | Read additional assumptions. As obtained from command line arguments.
assumptions :: Parseable f => Arguments -> IO [f]
assumptions arg = (parse parser . pack) `mapM` _assumptions arg


-- | Obtain goal formulas. Taken from command line arguments or standard input.
goals :: Parseable f => Arguments -> IO [f]
goals arg = case _goals arg of
    [] -> do
        --putStrLn "Reading goal formulas from standard input (end: CTRL-D)"
        parse (P.many1 parser) =<< getContents
    xs -> (parse parser . pack) `mapM` xs

