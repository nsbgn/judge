{-|
Module      : CLI
Description : Options to judge's command-line interface.
Copyright   : (c) 2017, 2018 N Steenbergen
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
module CLI where

import Prelude hiding (getContents)

import "base" Data.Char (toLower)
import "base" Data.List (intercalate)
import "base" Data.Monoid ((<>))
import "base" Data.Version (showVersion)
import "base" Control.Applicative ((*>),(<*),(<|>))
import "base" Control.Monad (void, forM)
import "base" System.Info (os)
import "base" System.IO (FilePath, IOMode(WriteMode))
import "base" System.IO.Error (catchIOError)
import "base" GHC.IO.Handle (Handle, hIsTerminalDevice)
import "base" GHC.IO.Handle.FD (stdout, stderr, stdin, openFile)
import "text" Data.Text (Text, pack, unpack)
import "text" Data.Text.IO (getContents)
import qualified "filepath" System.FilePath as FP
import qualified "directory" System.Directory as D
import qualified "optparse-applicative" Options.Applicative as O
import qualified "attoparsec" Data.Attoparsec.Text as P
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP

import Paths_judge (getDataDir, version) -- automatically generated
import Logic.Judge.Formula.Parser (parse, Parseable)
import qualified Logic.Judge.Writer as W

data Arguments = Arguments 
    { verbose      :: Bool
    , _goals       :: [String]
    , _assumptions :: [String]
    , _outfile     :: Maybe String
    , format       :: W.Format
    , _infile      :: String
    }


arguments :: IO Arguments
arguments = do
    logics <- map FP.takeBaseName <$> findLogics
    O.execParser (prog logics) 

    where

    prog logics = O.info
        (  O.helper <*> infoOptions logics <*> runOptions )
        (  O.fullDesc 
        <> O.progDescDoc (return description)
        <> O.header "judge - Decision procedure for formal logics" 
        <> O.footer "2017-2018, Utrecht University"
        )

    infoOptions logics = 
        O.infoOption 
            ("judge - version " ++ showVersion version)
            (  O.short 'V'
            <> O.long "version"
            <> O.help "Show version of the software and exit"
            ) 
        <*> O.infoOption 
            ("Available logical systems: " ++ intercalate ", " logics)
            (  O.short 'l'
            <> O.long "list-logics"
            <> O.help "List logical systems available in the data \n\
                      \directories and exit"
            ) 
    
    runOptions = Arguments
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


-- | Find all paths to logics in the resource directories.
findLogics :: IO [FilePath]
findLogics = do
    dat <- (FP.</> "logic") <$> getDataDir 
    xdg <- D.getXdgDirectory D.XdgData "judge"
    findFiles ["json", "yml", "yaml"] [xdg, dat]


-- | Find all files with the given extensions in the given directories. If a
-- directory does not exist or has permission issues, it is simply ignored.
findFiles :: [String] -> [FilePath] -> IO [FilePath]
findFiles extensions dirs = concat <$> forM dirs findFiles

    where
    -- | Find all relevant files in the given directory.
    findFiles :: FilePath -> IO [FilePath]
    findFiles dir = do 
        { files <- filter correctExtension <$> D.listDirectory dir
        ; return $ map (dir FP.</>) files
        } `catchIOError` (\e -> return [])


    -- | Check if the filepath has any of the accepted extensions.
    correctExtension :: FilePath -> Bool
    correctExtension path = flip any extensions $ 
        (/= Nothing) . 
        flip FP.stripExtension (map toLower path)
    


-- | Return input file name, or, if it doesn't exist, check if it matches any
-- name in the resource directories.
infile :: Arguments -> IO String
infile args = do
    let name = _infile args
    exists <- D.doesFileExist name
    if exists
    then return name
    else do
        logics <- filter (match name) <$> findLogics
        case logics of
            (filename:_) -> return filename
            _ -> return name

    where

    match :: String -> FilePath -> Bool
    match name path = name `elem` [FP.takeBaseName path, FP.takeFileName path]



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
        PP.text "Reading formulas from standard input" PP.<+>
        PP.lparen PP.<>
        eof PP.<+>
        PP.text "to finish" PP.<>
        PP.rparen

    eof :: PP.Doc
    eof = PP.bold . PP.text $ case os of
        "windows" -> "CTRL-Z"
        _ -> "CTRL-D"
