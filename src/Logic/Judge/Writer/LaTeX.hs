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
import "texmath" Text.TeXMath.TeX (renderTeX)
import "texmath" Text.TeXMath.Unicode.ToTeX (getTeXMath)
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
            PP.encloseSep PP.lbrace PP.rbrace PP.comma $ map (PP.text . unicode2tex) m
        ) <+> latex φ

instance Printable ext => LaTeX (F.Formula ext) where
    latex = PP.string . unicode2tex . show . PP.plain . pretty


instance (Printable ext) => LaTeX (T.Tableau ext) where
    latex θ = 
        PP.string "\\begin{forest}" <$> 
        PP.string "tableau" <$> 
        latex' θ <$> 
        PP.string "\\end{forest}"

        where
        latex' θ = case θ of
            T.Closure refs -> 
                PP.string ", closed={" <>
                cmd "n" (PP.tupled $ map PP.int refs) <>
                PP.string "}"
            T.Application name refs θs -> 
                PP.string ", apply=$\\sf " <> 
                PP.string (unicode2tex name) <+> 
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
    , "\\usepackage{forest,color}"
    , "\\forestset{"
    , "tableau/.style={"
    , "    for tree={"
    , "        parent anchor=south, child anchor=north,"
    , "        s sep=0.1cm, l sep=0.8cm, inner sep=0.2cm"
    , "    },"
    , "},"
    , "closed/.style={"
    , "    fit=band, label=below:{$\\otimes$ #1},"
    , "},"
    , "clamp/.style={"
    , "    no edge, before computing xy={l=\\baselineskip}"
    , "},"
    , "apply/.style={"
    , "    for last={"
    , "        edge label={"
    , "            node[very near end, anchor=south west, xshift=0.1cm, font=\\small]{#1}"
    , "        }"
    , "    }"
    , "},"
    , "}"
    , "\\newcommand{\\marked}[1]{\\texttt{\\footnotesize[#1]\\ }}"
    , "\\newcommand{\\n}[1]{\\textcolor{gray}{{\\tiny{#1}}}}"
    , "\\begin{document}"
    ]

{-
  declare toks register=closure,
  declare count register=level to prefix,
  prefix=X-,
  level to prefix=1,
  delay={
    for nodewalk/.process=Rw Rw
    {level to prefix}{level=#1}
    {prefix}{+content=#1}
  }
-}


-- | Footer for LaTeX output.
latexFooter :: PP.Doc
latexFooter = PP.text "\\end{document}"


-- | Convert Unicode strings (φ → ψ) to LaTeX (\psi \rightarrow \phi).
unicode2tex :: String -> String
unicode2tex str = stripHardSpaces $ getTeXMath str [] >>= flip renderTeX ""

    where
    -- Some hacks to change TeXMath's output to what I need
    stripHardSpaces :: String -> String
    stripHardSpaces string = case string of
        ('\\':'n':'e':'g':xs) -> "\\neg " ++ xs
        ('\\':' ':xs) -> ' ':stripHardSpaces xs
        (x:xs) -> x:stripHardSpaces xs
        [] -> []
