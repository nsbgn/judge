-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Printer
Description : Class instance and functions for printable structures.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Printer where

import Prelude hiding ((<$>))
import "base" Control.Monad (foldM)
import "base" GHC.IO.Handle (Handle)
import "base" GHC.IO.Handle.FD (stdout)
import "text" Data.Text (Text, pack, unpack)
import "terminal-size" System.Console.Terminal.Size (size, width)
import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP
import qualified "containers" Data.Tree as R
import qualified "containers" Data.Map as M1
import qualified "unordered-containers" Data.HashMap.Strict as M2

class Printable a where

    -- | Convert object into a text-based prettyprinted representation.
    pretty :: a -> PP.Doc


    -- | In some cases, the representation for a type must be embellished with 
    -- some other symbols when it occurs as part of a representation of a
    -- different type, but not when it occurs on its own. This printer allows 
    -- us to specify this alternative.
    prettyEmbedded :: a -> PP.Doc
    prettyEmbedded = pretty


    -- | Prettyprinting below the top level can optionally have a different
    -- procedure - for adding parentheses, for example.
    prettyRecursive :: a -> PP.Doc
    prettyRecursive = pretty


-- | Print a printable to stdout.
prettyprint :: Printable a => a -> IO ()
prettyprint = prettyprintH stdout


-- | Print a printable to some file handle.
prettyprintH :: Printable a => Handle -> a -> IO ()
prettyprintH fd x = do
    columns <- maybe 79 width `fmap` size
    PP.displayIO fd 
        . (PP.renderPretty 1.0 columns) 
        . (PP.<$$> PP.plain PP.empty) 
        . PP.fill columns
        . pretty 
        $ x

instance {-# OVERLAPPABLE #-} Printable a => Show a where
    show = show . pretty

instance {-# OVERLAPS #-} Printable String where
    pretty = phrase

instance Printable Bool where
    pretty True = PP.bold . PP.green . PP.char $ '✓'
    pretty False = PP.bold . PP.red . PP.char $ '×'


instance Printable Text where
    pretty = phrase . unpack


instance Printable Int where
    pretty = PP.int


instance (Printable a, Printable b) => Printable (Either a b) where
    pretty = either (left . pretty) (right . pretty)
        
        where
        left, right :: PP.Doc -> PP.Doc
        right = (<$$>) (PP.bold . PP.green . PP.text $ "Success:") . PP.indent 4
        left = (<$$>) (PP.bold . PP.red . PP.text $ "Failure:") . PP.indent 4

instance Printable a => Printable (Maybe a) where
    pretty = maybe PP.empty pretty

instance (Printable a, Printable b) => Printable (a,b) where
    pretty (x, y) = 
        PP.parens $ pretty x <+> PP.comma <+> pretty y

instance Printable a => Printable [a] where
    pretty = PP.vcat . map ((PP.char '-' <+>) . prettyRecursive)

instance Printable a => Printable (M1.Map String a) where
    pretty m = PP.vcat (M1.foldrWithKey foldEntry [] m)

instance Printable a => Printable (M2.HashMap String a) where
    pretty m = PP.vcat (M2.foldrWithKey foldEntry [] m)


instance Printable a => Printable (R.Tree a) where
    pretty (R.Node x []) = pretty x
    pretty (R.Node x xs) = pretty x <$> pretties xs

        where
        pretties trees' = case trees' of
            [] -> PP.empty
            [x] -> nest x
            (x:xs) -> nest x <$$> pretties xs

        nest x = PP.char '╷' <$$> PP.text "└── " <> PP.nest 4 (pretty x)



-- | Produce a document representing a comma-seperated list.
list :: Printable a => [a] -> PP.Doc
list [] = PP.empty
list xs = (PP.empty <+>) . PP.fillSep . PP.punctuate (PP.char ',') . map pretty $ xs


-- | Produce a document for each word.
phrase :: String -> PP.Doc
phrase = PP.fillSep . map PP.text . words


-- | Produce a string-seperated list.
seperates :: String -> [PP.Doc] -> PP.Doc
seperates str = PP.fillSep . PP.punctuate (PP.text $ ' ':str)


-- | Auxiliary: Fold function for String maps
foldEntry :: Printable a => String -> a -> [PP.Doc] -> [PP.Doc]
foldEntry k v acc = (PP.text k <+> PP.align (pretty v)) : acc
