-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Printer
Description : Prettyprinting instances for tableau systems and proofs.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Tableau.Printer () where

import Prelude hiding ((<$>))
import "text" Data.Text (Text, pack, unpack)
import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP
import qualified "containers" Data.Tree as R

import Logic.Judge.Tableau.Specification (Ref((:=)))
import Logic.Judge.Printer (Printable, pretty, prettyRecursive, phrase, seperates, list)
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Tableau.Specification as T
import qualified Logic.Judge.Tableau.Algorithm as TA

styleTitle = PP.bold 
styleSubtitle = PP.underline
styleName = PP.red . PP.dquotes
styleComment = PP.cyan
styleAnnotation = PP.magenta


instance Printable ext => Printable (TA.Tableau ext) where
    pretty θ = case θ of
        TA.Closure -> pretty False
        TA.Node φs subθ -> PP.vsep (map pretty φs) <$> pretty subθ
        TA.Application name refs θs -> branch
            (styleAnnotation $ pretty name <> list refs)
            (map pretty θs)
    
        where
        branch :: PP.Doc -> [PP.Doc] -> PP.Doc
        branch rule children = PP.vsep $ map (\child ->
            PP.char '╷' <+> rule <$> 
            PP.text "└── " <> PP.nest 4 child) children



instance (Printable input, Printable ext) => Printable (TA.Result input (TA.Tableau ext)) where
    pretty result = case result of
        TA.Failure input ->
            PP.red (PP.string "Failed to satisfy goal:") <+> pretty input
        TA.Success input output ->
            PP.green (PP.string "Success:") <$>
            pretty output



instance Printable b => Printable (Ref Int b) where
    pretty (i := v) = 
        pretty v <+> (styleAnnotation . PP.braces . pretty $ i)



instance (Printable ext, Printable primitive) => Printable (T.Terms primitive ext) where
    pretty terms = case terms of
        T.Primitive s -> pretty s
        T.Union ts -> "or" `seperates` map prettyRecursive ts
        T.Intersection ts -> "and simultaneously" `seperates` map prettyRecursive ts
        T.Transform s _ t -> phrase "one of the" <+> phrase s <+> phrase "of" <+> pretty t



instance Printable T.PrimitiveStaticTerms where
    pretty source = phrase $ case source of
        T.Root -> "a root node"
        T.Assumption -> "an assumption"


instance Printable T.PrimitiveDynamicTerms where
    pretty source = case source of
        T.Unprocessed -> phrase "an unprocessed node on the branch"
        T.Processed -> phrase "a processed node on the branch"
        T.Static s -> pretty s



instance (Printable ext, Printable primitive) => Printable (T.Constraint primitive ext) where

    pretty T.None = PP.empty
    pretty constraint = styleComment $ 
        PP.empty <$$> 
        phrase "where" <+> prettyRecursive constraint <$$> 
        PP.empty

    prettyRecursive constraint = case constraint of
        T.None -> PP.empty
        T.Match pattern terms -> pretty pattern <+> phrase "matches" <+> prettyRecursive terms
        T.Choose cs -> "or alternatively" `seperates` map prettyRecursive cs
        T.Merge cs -> "while simultaneously" `seperates` map prettyRecursive cs
    
    
instance (Printable ext) => Printable (T.RuleUninstantiated ext) where
    pretty T.Rule {T.name, T.productions, T.consumptions} =
        comment name <$$>
        comment "if the branch contains:" <$$>
        pretty consumptions <$$>
        PP.empty <$$>
        comment "then it may be extended with:" <$$>
        pretty (tree $ productions)
        
        where
        comment :: String -> PP.Doc
        comment = styleComment . phrase

        -- | Represent a conjunction of disjunctions as a tree.
        tree :: [[a]] -> R.Tree (Maybe a)
        tree = R.Node Nothing . map conjunctiveTree
       
        -- | Represent a conjunction as a tree.
        conjunctiveTree :: [a] -> R.Tree (Maybe a)
        conjunctiveTree [] = R.Node Nothing []
        conjunctiveTree [x] = R.Node (Just x) []
        conjunctiveTree (x:xs) = R.Node (Just x) [conjunctiveTree xs]
                 

instance (Printable ext) => Printable (T.TableauSystem ext) where
    pretty tableau = 
        (styleTitle . PP.text $ "Tableau for logic " ++ T.title tableau) <$$>
        PP.empty <$$>
        PP.indent 2 (
            subtitle "Assumptions" <$$> 
            PP.empty <$$>
            pretty (T.assumptions tableau) <$$>
            PP.empty <$$>
            subtitle "Rules" <$$> 
            PP.empty <$$>
            pretty (T.rules tableau) <$$>
            PP.empty
        )

        where 
        subtitle = styleSubtitle . PP.text
        title = styleTitle . PP.text


