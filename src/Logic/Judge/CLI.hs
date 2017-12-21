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

import "base" System.Info (os)
import "base" System.IO (IOMode(WriteMode))
import "base" Data.Monoid ((<>))
import "base" Control.Applicative ((*>),(<*))
import "base" Control.Monad (void)
import "base" GHC.IO.Handle (Handle, hIsTerminalDevice)
import "base" GHC.IO.Handle.FD (stdout, stderr, stdin, openFile)
import qualified "optparse-applicative" Options.Applicative as O
import qualified "attoparsec" Data.Attoparsec.Text as P
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP

import Logic.Judge.Formula.Parser (parse, Parseable)
import qualified Logic.Judge.Writer as W

data Arguments = Arguments 
    { verbose      :: Bool
    , _goals       :: [String]
    , _assumptions :: [String]
    , _outfile     :: Maybe String
    , format       :: W.Format
    , infile       :: String
    }



arguments :: IO Arguments
arguments = O.execParser prog 

    where

    prog = O.info
        (  O.helper <*> options )
        (  O.fullDesc 
        <> O.progDescDoc (return description)
        <> O.header "judge - Decision procedure for formal logics" 
        <> O.footer "2017, Utrecht University"
        )

    options = Arguments
        <$> O.switch 
            (  O.short 'v'
            <> O.long "verbose"
            <> O.help "Show diagnostics"
            ) 
        <*> O.many
            ( O.strOption
                ( O.short 'g'
                <> O.long "goal"
                <> O.metavar "EXPR"
                <> O.help "Set target formula(s). May be provided multiple \n\
                          \times (default: standard input)"
                )
            )
        <*> O.many
            ( O.strOption
                ( O.short 'a'
                <> O.long "assumption"
                <> O.metavar "EXPR"
                <> O.help "Add assumption(s). May be provided multiple times"
                )
            )
        <*> O.optional
            ( O.strOption
                (  O.short 'o'
                <> O.long "output"
                <> O.metavar "PATH"
                <> O.help "Output file (default: standard output)"
                )
            )
        <*> ( O.option O.auto
                (  O.short 'f' 
                <> O.long "format" 
                <> O.metavar "FORMAT"
                <> O.value W.Plain 
                <> O.showDefault
                <> O.help "Output format"
                )
            )
        <*> ( O.argument O.str (O.metavar "LOGIC"))

    description :: PP.Doc
    description = 
        (PP.line <>) 
        . PP.fillSep 
        . map PP.text 
        . words 
        $ "Decides whether given logical formulas are provable in some \n\
          \logical system. Takes a YAML or JSON file as input. Refer to \n\
          \README.md for more information."


-- | Obtain and open file handle for output file.
outfile :: Arguments -> IO Handle
outfile arg = maybe (return stdout) (flip openFile WriteMode) (_outfile arg)


-- | Obtain additional assumptions. Taken from command line arguments.
assumptions :: Parseable f => Arguments -> IO [f]
assumptions arg = mapM (parse . pack) (_assumptions arg)


-- | Obtain goal formulas. Taken from command line arguments or standard input.
goals :: Parseable f => Arguments -> IO [f]
goals arg = case _goals arg of
    [] -> do
        terminal <- hIsTerminalDevice stdin
        if terminal
            then W.prettyprint stderr notification
            else return ()
        getContents >>= parse
    xs -> mapM (parse . pack) xs

    where
    notification :: PP.Doc
    notification = 
        PP.text "Reading from standard input" PP.<+>
        PP.lparen PP.<>
        eof PP.<+>
        PP.text "to finish" PP.<>
        PP.rparen

    eof :: PP.Doc
    eof = PP.bold . PP.text $ case os of
        "windows" -> "CTRL-Z"
        _ -> "CTRL-D"
