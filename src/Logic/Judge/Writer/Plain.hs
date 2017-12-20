-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Writer.Plain
Description : Class instance and functions for printable structures.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Writer.Plain where

import Prelude hiding ((<$>))
import "base" Control.Monad (foldM)

import "text" Data.Text (Text, pack, unpack)
import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP
import qualified "containers" Data.Tree as R
import qualified "containers" Data.Map as M1
import qualified "unordered-containers" Data.HashMap.Strict as M2

import qualified Logic.Judge.Formula.Datastructure as F
import Logic.Judge.Prover.Tableau (Ref((:=)))
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Prover.Tableau as T

styleForm = id
styleOp = PP.bold
styleVar = PP.green
styleConst = id
styleMark = PP.yellow

styleTitle = PP.bold 
styleSubtitle = PP.underline
styleName = PP.red . PP.dquotes
styleComment = PP.cyan
styleAnnotation = PP.magenta

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


instance Printable a => Printable (Maybe a) where
    pretty = maybe PP.empty pretty

instance (Printable a, Printable b) => Printable (Either a b) where
    pretty = either (left . pretty) (right . pretty)
        
        where
        left, right :: PP.Doc -> PP.Doc
        right = (<$$>) (PP.bold . PP.green . PP.text $ "Success:") . PP.indent 4
        left = (<$$>) (PP.bold . PP.red . PP.text $ "Failure:") . PP.indent 4


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











instance Printable f => Printable (F.Marked f) where
    pretty (F.Marked [] formula) = pretty formula
    pretty (F.Marked marks formula) = prettify marks <+> pretty formula 
        where
        prettify = 
            styleMark . PP.brackets . PP.cat . 
            PP.punctuate (PP.comma <> PP.space) . 
            map PP.text


instance Printable term => Printable (F.Ambiguous term) where
    pretty (F.Ambiguous (t:_)) = pretty t
    pretty (F.Ambiguous []) = error $
        "Ambiguous term has no readings. Please report this as a bug."


instance Printable ext => Printable (F.Term ext) where
    pretty (F.Formula f) = pretty f
    pretty (F.Extension e) = pretty e
    pretty (F.MarkedFormula f) = pretty f


instance (Printable ext) => Printable (F.Formula ext) where
    prettyRecursive el = styleForm $ case el of
        F.Constant _        -> pretty el
        F.Variable _        -> pretty el
        F.Negation _        -> pretty el
        _                   -> PP.parens $ pretty el

    pretty el = styleForm $ case el of
        F.Constant True     -> styleConst $ PP.char '⊤'
        F.Constant False    -> styleConst $ PP.char '⊥'
        F.Variable s        -> styleVar $ PP.text s
        F.Negation p        -> unary '¬' p
        F.Disjunction p q   -> binary p '∨' q
        F.Conjunction p q   -> binary p '∧' q
        F.XDisjunction p q  -> binary p '⊻' q
        F.Implication p q   -> binary p '→' q
        F.BiImplication p q -> binary p '↔' q
        F.Extend j p        -> prettyEmbedded j <> prettyRecursive p



instance Printable F.Justification where

    prettyEmbedded x = pretty x <> (styleOp $ PP.text " : ")
  
    pretty el = styleForm $ case el of
        F.ProofConstant s -> styleConst $ PP.text s
        F.ProofVariable s -> styleVar $ PP.text s
        F.ProofChecker j  -> unary '!' j
        F.Application j k -> binary j '⋅' k
        F.Sum j k         -> binary j '+' k

  
    prettyRecursive el = case el of
        F.Application j k -> PP.parens $ pretty el
        F.Sum j k         -> PP.parens $ pretty el
        _                 -> pretty el
       


instance Printable F.Modality where
    pretty el = PP.char $ case el of
        F.Necessary -> '□'
        F.Possible  -> '◇'


        
instance Printable F.Quantifier where
    pretty el = PP.text $ case el of
        F.Universal   x -> "∀" ++ x ++ ". "
        F.Existential x -> "∃" ++ x ++ ". "



-- | Helper for prettyprinting binary operators.
unary :: Printable a => Char -> a -> PP.Doc
unary c p = (styleOp . PP.char) c <> prettyRecursive p


-- | Helper for prettyprinting binary operators.
binary :: (Printable a, Printable b) => a -> Char -> b -> PP.Doc
binary p c q = 
    prettyRecursive p <+> 
    (styleOp $ PP.char c) <+> 
    prettyRecursive q



instance Printable ext => Printable (T.Tableau ext) where
    pretty θ = case θ of
        T.Closure -> pretty False
        T.Node φs subθ -> PP.vsep (map pretty φs) <$> pretty subθ
        T.Application name refs θs -> branch
            (styleAnnotation $ pretty name <> list refs)
            (map pretty θs)
    
        where
        branch :: PP.Doc -> [PP.Doc] -> PP.Doc
        branch rule children = PP.vsep $ map (\child ->
            PP.char '╷' <+> rule <$> 
            PP.text "└── " <> PP.nest 4 child) children



instance (Printable input, Printable ext) => Printable (T.Result input (T.Tableau ext)) where
    pretty result = case result of
        T.Failure input ->
            PP.red (PP.string "Failed to satisfy goal:") <+> pretty input
        T.Success input output ->
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
        T.Bind pattern terms -> pretty pattern <+> phrase "matches" <+> prettyRecursive terms
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
            pretty (T.assumptions' tableau) <$$>
            PP.empty <$$>
            subtitle "Rules" <$$> 
            PP.empty <$$>
            pretty (T.rules tableau) <$$>
            PP.empty
        )

        where 
        subtitle = styleSubtitle . PP.text
        title = styleTitle . PP.text


