-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Writer.LaTeX
Description : Instances for LaTeX output.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Logic.Judge.Writer.LaTeX where

import Prelude hiding ((<$>))
import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ((<>), (<+>), (</>), (<$>), (<$$>), (<//>))
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen as PP

import Logic.Judge.Writer.Plain (Printable, pretty)
import Logic.Judge.Prover.Tableau (Ref((:=)))
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Prover.Tableau as T


class LaTeX a where
    latex :: a -> PP.Doc



instance (LaTeX input, Printable ext) => LaTeX (T.Result input (T.Tableau ext)) where
    latex result = wrap $ case result of
        T.Failure input ->
            PP.string "Failed to satisfy goal: $" <+> latex input <> PP.char '$'
        T.Success input output ->
            latex output

        where
        wrap result = 
            PP.string "\\begin{result}" <$> 
            result <$> 
            PP.string "\\end{result}"



instance LaTeX a => LaTeX (Ref Int a) where
    latex (i := φ) = 
        PP.char '$' <> 
        latex φ <> 
        PP.char '$' <+> 
        cmd "n" (PP.int i)


instance LaTeX a => LaTeX (F.Marked a) where
    latex (F.Marked m φ) = 
        cmd "marked" (
            PP.encloseSep PP.lbrace PP.rbrace PP.comma $ map PP.text m
        ) <+> latex φ

instance Printable ext => LaTeX (F.Formula ext) where
    latex = pretty

instance (Printable ext) => LaTeX (T.Tableau ext) where
    latex θ = 
        PP.string "\\begin{forest}" <$> 
        PP.string "tableau" <$> 
        latex' θ <$> 
        PP.string "\\end{forest}"

        where
        latex' θ = case θ of
            T.Closure -> 
                PP.string ", closed"
            T.Application name refs θs -> 
                PP.string ", apply=$\\sf " <> 
                PP.string name <+> 
                PP.string "$\\ " <> 
                cmd "n" (PP.tupled $ map PP.int refs) <$> 
                PP.indent 4 (PP.vsep $ map latex' θs)
            T.Node (φ:φs) θ -> 
                PP.lbracket <+> latex φ <> 
                foldr (\φ doc -> 
                    PP.line <> PP.indent 4 (
                        PP.lbracket <+> latex φ <> PP.string ", clamp" <> 
                        doc <$> 
                        PP.rbracket
                    )
                ) (latex' θ) φs <$> 
                PP.rbracket


-- | Convenience function for writing LaTeX commands.
cmd :: String -> PP.Doc -> PP.Doc
cmd s doc = PP.char '\\' <> PP.string s <> PP.lbrace <> doc <> PP.rbrace

-- | Header for LaTeX output. Eventually use templates instead of inlining. 
latexHeader :: PP.Doc
latexHeader = PP.vsep $ map PP.string 
    [ "\\documentclass[multi=result,margin=1cm]{standalone}"
    , "\\usepackage{tikz,forest,color,unicode-math}"
    , "\\forestset{"
    , "tableau/.style={"
    , "    for tree={"
    , "        parent anchor=south,"
    , "        child anchor=north,"
    , "        s sep=0.3cm,"
    , "        l sep=0.7cm,"
    , "        inner sep=0.1cm"
    , "    },"
    , "},"
    , "closed/.style={"
    , "    label=below:$\\otimes$"
    , "},"
    , "clamp/.style={"
    , "    no edge,"
    , "    before computing xy={l=\\baselineskip}"
    , "},"
    , "apply/.style={"
    , "    for last={"
    , "        edge label={"
    , "            %node[near end, above right, xshift=0.1cm,font=\\small]{#1}"
    , "            node[very near end, anchor=south west, xshift=0.1cm, font=\\small]{#1}"
    , "        }"
    , "    }"
    , "},"
    , "}"
    , "\\newcommand{\\marked}[1]{\\texttt{\\footnotesize[#1]\\ }}"
    , "\\newcommand{\\n}[1]{\\textcolor{gray}{{\\tiny{#1}}}}"
    , "\\begin{document}"
    ]



-- | Footer for LaTeX output.
latexFooter :: PP.Doc
latexFooter = PP.text "\\end{document}"

