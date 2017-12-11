-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Formula.Datastructure
Description : Plain datastructures and operations on logical formulas.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Formula.Datastructure where

import "base" Data.List (nub)

-- | For our purposes, a @Formula@ is a structure that is built upon a formula
-- of classical propositional logic. It has all the standard connectives, plus
-- an optional @Extension@ that may hold quantifiers, modalities, etcetera.
--
-- Note that I experimented with parametrising over the variable type, for
-- immediate compatibility with the @Control.Unification@ module from
-- @unification-fd@, but it threatened to make the code unnecessarily
-- complicated.
--
-- Note also that it is generally expected that formulas will be 'simplify'ed
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



-- | Classical propositional logic has no extension.
type Classical = ()
type Proposition = Formula Classical



-- | Predicate logic is extended with quantifiers (and relation symbols,
-- unimplemented).
data Quantifier
    = Universal String -- ∀x (Unicode: Mathematical Operators)
    | Existential String -- ∃x (Unicode: Mathematical Operators)
    deriving (Eq)

type Predicate = Formula Quantifier


-- | Standard modal logics have two (dual) unary modal operators.
data Modality
    = Necessary -- □, K, ... (Unicode: Geometric Shapes)
    | Possible -- ◇, B, ... (Unicode: Geometric Shapes)
    deriving (Eq, Ord)

type FormulaML = Formula Modality



-- | Justification logics are extended with justification terms.
data Justification
    = ProofVariable String -- x, y, z...
    | ProofConstant String -- a, b, c...
    | ProofChecker Justification -- !
    | Application Justification Justification -- ⋅ (Unicode: Latin-1 Supplement)
    | Sum Justification Justification -- +
    deriving (Eq, Ord)

type FormulaJL = Formula Justification


-- MARKS ---------------------------------------------------------------------

-- | A marked formula is simply a formula with zero or more string annotations.
-- This makes for easy generalisation: marks can carry information that is
-- common to tableau methods (namely, the polarity of the formula in the
-- current node), as well as information that is demanded specifically by
-- justification logic tableaus.
data Marked formula = Marked 
    { marks :: [String]
    , unmarked :: formula
    } deriving (Eq, Ord)

instance Functor Marked where
    fmap f (Marked marks x) = Marked marks (f x)

mark :: [String] -> Marked a -> Marked a
mark new (Marked old x) = Marked (new ++ old) x



-- BASIC MANIPULATIONS -------------------------------------------------------

-- | Simplify formulae to only falsehood, implication and justification. This
-- reduces the number of rules that need implementation.
--
-- TODO: This should not be hardcoded, but read from configuration
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



-- | Turn a formula into its direct negation by adding or stripping away a
-- negation symbol.
negation :: Formula ext -> Formula ext
negation (Implication x (Constant False)) = x
negation (Negation x) = x
negation (Constant True) = Constant False
negation (Constant False) = Constant True
negation x = Negation x



-- | Check if formulas are in direct (!) contradiction with eachother, that is,
-- if there is a formula φ in one of the sets such that there is a formula 
-- φ → ⊥ or a formula ¬φ in the other set.
--
-- To test if a branch is really closed, it is only necessary to check if the
-- *newly added* formulas conflict with the *remaining* formulas; it's not
-- necessary to check for conflict in all formulas. Since there will only be a
-- few formulas new, we will assume that the first argument is smaller than the
-- second.
--
-- Assume that ys is NOT self-contradicting.
contradict :: (Traversable t1, Traversable t2, Eq ext) 
           => t1 (Formula ext) 
           -> t2 (Formula ext) 
           -> Bool
contradict xs ys = 
    Constant False `elem` xs ||
    any (`elem` ys) (fmap negation xs)



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


-- | Interpret a marked formula as a choice of terms.
asTerm :: Marked (Formula ext) -> [Term ext]
asTerm φ = [MarkedFormula φ, Formula . unmarked $ φ]



-- | A parsed term may be ambiguous: "S" can be parsed as a Formula or as a
-- Justification. Such ambiguous are stored in an Ambiguous type to be resolved
-- later.
newtype Ambiguous term = Ambiguous [term]



-- | The 'Subterm' class represents a relation between terms based on an
-- extension @ext@ (that is, formulas or extensions of formulas) and subterms 
-- that may occur within those @ext@-terms.
class (Subterm ext) term where
    
    -- | Return all the subterms occurring in a term. Note: May contain
    -- duplicates.
    subterms :: term -> [Term ext]

{-
    -- | Return the nesting depth of the term.
    depth :: term -> Int

    -- | Return the size of the term.
    size :: term -> Int
-}

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


-- TODO: Along with 'subterms', this should be a 'set' so that we don't get
-- duplicates
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


instance HasVariables ext => HasVariables (Ambiguous (Term ext)) where
    variables (Ambiguous terms) = terms >>= variables

    isVariable (Ambiguous terms) = any isConstant terms

    isConstant (Ambiguous terms) = any isConstant terms


instance HasVariables term => HasVariables (Marked term) where
    variables (Marked _ f) = variables f

    isVariable (Marked _ f) = isVariable f

    isConstant (Marked _ f) = isConstant f

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