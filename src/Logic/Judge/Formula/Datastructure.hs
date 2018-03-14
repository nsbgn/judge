{-|
Module      : Logic.Judge.Formula.Datastructure
Description : Basic datastructures and instances.
Copyright   : (c) 2017, 2018 N Steenbergen
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

Plain datastructures, class instances and operations on logical formulas.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Formula.Datastructure (
    -- * Datastructures
    -- ** Formulas
      Formula(..)
    , Marked(..)
    , Term(..)
    , Ambiguous(..)
    -- ** Extensions
    , Proposition
    , Predicate
    , FormulaML
    , FormulaJL
    , Classical
    , Quantifier(..)
    , Modality(..)
    , Justification(..)
    -- * Operations
    , simplify
    , asTerm
    , isFormula
    , isExtension
    , isMarkedFormula
    -- * Classes
    , Subterm(..)
    , HasVariables(..)
    ) where

import "base" Data.List (nub)

-- | For our purposes, a @Formula@ is a structure that is built upon a formula
-- of classical propositional logic. It has all the standard connectives, plus
-- an optional 'Extension' that may hold quantifiers, modalities, etcetera.
--
-- Note that it is generally expected that formulas will be 'simplify'ed
-- before being processed.
data Formula ext
    = Variable      String                      -- a, b, c...
    | Constant      Bool                        -- ⊥, ⊤ (Unicode: Mathematical Operators)
    | Extend        ext           (Formula ext) -- :
    | Negation      (Formula ext)               -- ¬~ (Unicode: Latin-1 Supplement)
    | Conjunction   (Formula ext) (Formula ext) -- ∧&
    | Disjunction   (Formula ext) (Formula ext) -- ∨|
    | XDisjunction  (Formula ext) (Formula ext) -- ⊻^
    | Implication   (Formula ext) (Formula ext) -- →->  (Unicode: Arrows)
    | BiImplication (Formula ext) (Formula ext) -- <->↔ (Unicode: Arrows)
    deriving (Eq, Ord)


-- | Formulas of propositional logic.
type Proposition = Formula Classical

-- | Formulas of predicate logic.
type Predicate = Formula Quantifier 

-- | Formulas of modal logic.
type FormulaML = Formula Modality 

-- | Formulas of justification logic.
type FormulaJL = Formula Justification 


-- | The formula extension for classical propositional logic is empty.
type Classical = ()

-- | Predicate logic is extended with quantifiers (and relation symbols,
-- unimplemented).
data Quantifier
    = Universal String -- ∀x (Unicode: Mathematical Operators)
    | Existential String -- ∃x (Unicode: Mathematical Operators)
    deriving (Eq)


-- | Standard modal logics have two (dual) unary modal operators.
data Modality
    = Necessary -- □, K, ... (Unicode: Geometric Shapes)
    | Possible -- ◇, B, ... (Unicode: Geometric Shapes)
    deriving (Eq, Ord)


-- | Justification logics are extended with justification terms.
data Justification
    = ProofVariable String -- x, y, z...
    | ProofConstant String -- a, b, c...
    | ProofChecker Justification -- !
    | Application Justification Justification -- ⋅ (Unicode: Latin-1 Supplement)
    | Sum Justification Justification -- +
    deriving (Eq, Ord)


-- MARKS ---------------------------------------------------------------------

-- | A marked formula is simply a formula with zero or more string annotations.
-- This makes for easy generalisation: marks can carry the polarity of a
-- formula, as well as state information specific to a particular tableau
-- system.
data Marked formula = Marked 
    { marks :: [String]
    , unmarked :: formula
    } deriving (Eq, Ord)

instance Functor Marked where
    fmap f (Marked marks x) = Marked marks (f x)



-- BASIC MANIPULATIONS -------------------------------------------------------

-- | Simplify formulae to only falsehood, implication and justification. This
-- reduces the number of rules that need implementation.
simplify :: Formula ext -> Formula ext
simplify formula = case formula of
    Variable v -> Variable v
    Constant False -> Constant False
    Constant True -> Implication (Constant False) (Constant False)
    Extend e f -> Extend e (simplify f)
    Implication f1 f2 -> Implication (simplify f1) (simplify f2)
    Negation f -> Implication (simplify f) (Constant False)
    Disjunction f1 f2 -> simplify $ Implication (Negation f1) f2
    Conjunction f1 f2 -> simplify $ Negation (Implication f1 (Negation f2)) 
    XDisjunction f1 f2 -> simplify $ Conjunction (Implication (Negation f1) f2) (Implication (Negation f2) f1)
    BiImplication f1 f2 -> simplify $ Conjunction (Implication f1 f2) (Implication f2 f1)



-- SUBTERMS ------------------------------------------------------------------

-- | The term datastructure disambiguates between terms of the logical language
-- and terms of the logical extension language (e.g. justifications).
--
-- The alternative to carrying this information at the value level is to have 
-- a multi-parameter @Substructure sub base@ class relative to which operations 
-- like @pattern@ing or the @occurs@ check are defined. Although that seemed 
-- prettier in theory, it made the code a whole lot more complicated.
data Term ext
    = Formula (Formula ext)
    | Extension ext
    | MarkedFormula (Marked (Formula ext))
    deriving (Eq, Ord)

-- | Return true if and only if the term is a formula.
isFormula :: Term ext -> Bool
isFormula (Formula f) = True
isFormula _ = False

-- | Return true iff the term is a formula extension.
isExtension :: Term ext -> Bool
isExtension (Extension e) = True
isExtension _ = False

-- | Return true iff the term is a marked formula.
isMarkedFormula :: Term ext -> Bool
isMarkedFormula (MarkedFormula _) = True
isMarkedFormula _ = False


-- | Interpret a marked formula as a choice of terms. Note that it is not
-- always clear whether a value from 'Terms' is meant as the marked or the
-- unmarked version — so we offer both.
asTerm :: Marked (Formula ext) -> [Term ext]
asTerm φ = [MarkedFormula φ, Formula . unmarked $ φ]



-- | A parsed term may be ambiguous: "S" can be parsed as a Formula or as a
-- Justification. Such ambiguous are stored in an Ambiguous type to be resolved
-- later.
newtype Ambiguous term = Ambiguous [term]



-- | The @Subterm@ class represents a relation between terms based on an
-- extension @ext@ (that is, formulas or extensions of formulas) and subterms 
-- that may occur within those @ext@-terms.
class (Subterm ext) term where
    
    -- | Return all the subterms occurring in a term. Note: May contain
    -- duplicates.
    subterms :: term -> [Term ext]

instance Subterm ext ext => Subterm ext (Term ext) where
    subterms (Formula f) = subterms f
    subterms (Extension e) = subterms e
    subterms (MarkedFormula f) = subterms f

instance Subterm ext ext => Subterm ext (Marked (Formula ext)) where
    subterms t@(Marked _ f) = (MarkedFormula t) : subterms f

instance Subterm ext ext => Subterm ext (Ambiguous (Term ext)) where
    subterms (Ambiguous terms) = terms >>= subterms

instance Subterm ext ext => Subterm ext (Formula ext) where
    subterms term = case term of
        t@(Variable var) -> [Formula t]
        t@(Constant a) -> [Formula t]
        t@(Implication a b) -> Formula t:subterms a ++ subterms b
        t@(Extend e a) -> Formula t:subterms e ++ subterms a

instance Subterm Justification Justification where
    subterms term = case term of
        t@(ProofVariable var) -> [Extension t]
        t@(ProofConstant c)   -> [Extension t]
        t@(ProofChecker s)    -> Extension t:subterms s
        t@(Application s u)   -> Extension t:subterms s ++ subterms u
        t@(Sum s u)           -> Extension t:subterms s ++ subterms u


-- | The @HasVariables@ class is applicable to formulas and formula extensions
-- that consist of substructures with variables and constants, and operators to
-- combine them.
class HasVariables term where
    -- | Return the variables occurring in a term. Note: May contain
    -- duplicates.
    variables :: term -> [String]

    -- | Return true if the term is a variable.
    isVariable :: term -> Bool

    -- | Return true if the term is a constant.
    isConstant :: term -> Bool

    -- | Return true if the term is atomary.
    isAtomary :: term -> Bool
    isAtomary t = isConstant t || isVariable t

    -- | Return the number of operators in the term.
    size :: term -> Int


instance HasVariables ext => HasVariables (Term ext) where
    variables (Formula f) = variables f
    variables (Extension e) = variables e
    variables (MarkedFormula f) = variables f

    isVariable (Formula f) = isVariable f
    isVariable (Extension e) = isVariable e
    isVariable (MarkedFormula f) = False

    isConstant (Formula f) = isConstant f
    isConstant (Extension e) = isConstant e
    isConstant (MarkedFormula f) = False

    size (Formula f) = size f
    size (Extension e) = size e
    size (MarkedFormula f) = size f


instance HasVariables ext => HasVariables (Ambiguous (Term ext)) where
    variables (Ambiguous terms) = terms >>= variables

    isVariable (Ambiguous terms) = any isConstant terms

    isConstant (Ambiguous terms) = any isConstant terms

    size (Ambiguous []) = 0
    size (Ambiguous (t:_)) = size t

instance HasVariables term => HasVariables (Marked term) where
    variables (Marked _ f) = variables f

    isVariable (Marked _ f) = isVariable f

    isConstant (Marked _ f) = isConstant f

    size (Marked m f) = length m + size f

instance HasVariables ext => HasVariables (Formula ext) where
    variables term = case term of
        Variable var -> [var]
        Constant a -> []
        Implication a b -> variables a ++ variables b
        Extend e a -> variables e ++ variables a

    isVariable (Variable _) = True
    isVariable _ = False

    isConstant (Constant _) = True
    isConstant _ = False

    size term = case term of
        Implication a b -> 1 + size a + size b
        Extend a b -> 1 + size a + size b
        _ -> 0

instance HasVariables Justification where
    variables term = case term of
        ProofVariable var -> [var]
        ProofConstant c   -> []
        ProofChecker s    -> variables s
        Application s u   -> variables s ++ variables u
        Sum s u           -> variables s ++ variables u

    isVariable (ProofVariable _) = True
    isVariable _ = True

    isConstant (ProofConstant _) = True
    isConstant _ = False

    size term = case term of
        ProofChecker s -> 1 + size s
        Application a b -> 1 + size a + size b
        Sum a b -> 1 + size a + size b
        _ -> 0
