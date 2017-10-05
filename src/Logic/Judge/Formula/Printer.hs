-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Formula.Printer
Description : Prettyprinting instances for logical formulas.
License     : GPL-3
Stability   : experimental
-}

module Logic.Judge.Formula.Printer () where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Logic.Judge.Printer (Printable, pretty, prettyEmbedded, prettyRecursive)
import qualified Logic.Judge.Formula.Datastructure as F


styleForm = id
styleOp = PP.bold
styleVar = PP.green
styleConst = id
styleMark = PP.yellow


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
