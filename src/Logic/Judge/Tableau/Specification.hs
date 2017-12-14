-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Specification
Description : Preprocesses the input to the tableau algorithm.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Logic.Judge.Tableau.Specification where

import "base" Debug.Trace (trace, traceShow, traceM, traceShowM)

import "base" Data.Maybe (mapMaybe, catMaybes)
import "base" Data.List (nub, intersect, (\\))
import "base" Control.Monad (foldM, guard)
import "transformers" Control.Monad.Trans.State.Lazy (StateT(StateT), execStateT, get, put)
import qualified "containers" Data.Map as M

import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Formula.Substitution as Fσ

type Pattern ext = F.Ambiguous (F.Term ext)

data TableauSystem ext = TableauSystem
    { name          :: String
    , rules         :: [RuleUninstantiated ext]
    , assumptions   :: [F.Formula ext]
    }


-- | Relates values to their identifiers.
data Ref ref val = (:=) { reference :: ref, value :: val }
infixr 7 :=

-- | The base rule can represent both instantiated and uninstantiated tableau
-- rules.
data Rule generator ext = Rule 
    { consumptions :: [ F.Marked (F.Formula ext) ]
    -- ^ The consumptions (also: premises, antecedents, conditions) are
    -- formulas that are to be present on the branch before the rule may be
    -- applied.
    , productions :: [[ F.Marked (F.Formula ext) ]]
    -- ^ The productions (also: conclusions, consequents, results) are the
    -- formulas that will be created on the branch when the rule is applied. 
    -- Represents a disjunction of conjunctions.
    , generator :: generator
    -- ^ A generator is a 'permissive constraint', which represents a choice 
    -- between possible variable assignments. This approach is necessary to be
    -- able to handle free variables in the productions: such variables
    -- do not have a pre-existing binding to check for compliance, so they 
    -- need to be created. This also makes it possible to keep track of which 
    -- bindings have already been attempted over the course of an algorithm,
    -- thus allowing for certain termination guarantees in case such 
    -- guarantees cannot be achieved otherwise.
    -- 
    -- The limitation of the generator is that no variable may be bound to 
    -- terms from a dynamic set.
    -- Note that a rule with an empty generator is no longer useful.
    , constraint :: Constraint PrimitiveDynamicTerms ext
    -- ^ Although the generator *does* also restrict bound variables (with 
    -- brute force: a variable's previous binding will block all conflicting 
    -- assignments), it is more computationally efficient to simply check 
    -- already known values for compliance, during runtime.
    --
    -- The limitation of prohibitive constraints is that they cannot deal with
    -- 'free' variables. The intersection of generators and constraints 
    -- alleviates both their limitations.
    , compositor :: Compositor
    -- ^ The compositor indicates how to handle the case where multiple 
    -- instances are suggested by the generator.
    }



-- | Indicates how to handle rule instantiations.
data Compositor = Greedy | Nondeterministic


-- | An uninstantiated rule.
type RuleUninstantiated ext = Ref String (Rule (Constraint PrimitiveStaticTerms ext) ext)


-- | An instantiated tableau rule.
type RuleInstantiated ext = Ref String (Rule (L.PointedList (Fσ.Substitution ext)) ext)


-- | Represent sets of primitive source formulas to be used in constraints. 
data PrimitiveDynamicTerms 
    = Static PrimitiveStaticTerms
 -- | 'Active' terms currently not processed on the branch. 
    | Processed
 -- | 'Inactive' terms currently processed on the branch.
    | Unprocessed


-- | Represent sets of primitive source formulas to be used in constraints or 
-- generators. Note that a generator can only use "static" sources.
data PrimitiveStaticTerms 
 -- | Goal formula.
    = Root
 -- | Assumption formulas or constant specification.
    | Assumption


-- | Represent complex sets of source terms, to be turned into concrete terms
-- at a point when they are known.
--
-- When concretising terms, note when there are both marked and unmarked
-- versions, such as for 'root', produce both of them.
data Terms primitive ext
    = Primitive primitive
 -- | Keep terms that occur in at least one constituent.
    | Union [Terms primitive ext]
 -- | Keep only terms that occur in all constituents.
    | Intersection [Terms primitive ext]
 -- | Apply a transformation to terms.
    | Transform String ([F.Term ext] -> [F.Term ext]) (Terms primitive ext)
 -- | Filter (but don't bind) terms satisfying some pattern. (TODO)
 -- | Filter (F.Pattern ext) (TermsSpecification ext)

type DynamicTerms = Terms PrimitiveDynamicTerms
type StaticTerms = Terms PrimitiveStaticTerms



-- | A constraint is placed on a tableau rule to restrict the values to which
-- its variables can be bound. This means that some applications of the rule
-- will be blocked; but also that any 'free' or 'generative' variables (that 
-- is, variables that occur in the rule's productions but not in its
-- consumptions) can now be associated with a set of possible assignments, 
-- thereby making it possible to, essentially, generate a *choice* of multiple 
-- *instantiations* of a singular rule.
data Constraint primitive ext
    = None
 -- | Bind or constrain variables from a pattern to all terms that match the 
 -- pattern.
    | Match (Pattern ext) (Terms primitive ext)
 -- | Constraint holds if one of the subconstraints hold.
    | Choose [Constraint primitive ext]
 -- | Constraint holds if all subconstraints hold.
    | Merge [Constraint primitive ext]


-- | Check that a variable assignment does not conflict with a constraint.
respects :: forall ext . (F.Extension ext)
         => (DynamicTerms ext -> [F.Term ext])
         -> Fσ.Substitution ext
         -> Constraint PrimitiveDynamicTerms ext
         -> Bool
respects concretise σ ι = case ι of
    None           -> True
    Choose ιs      -> any (respects concretise σ) ιs 
    Merge ιs       -> all (respects concretise σ) ιs
    Match scheme τ -> anyMatching scheme τ

    where 
    anyMatching :: F.Ambiguous (F.Term ext) -> DynamicTerms ext -> Bool
    anyMatching (F.Ambiguous schemes) τ = not . null . catMaybes $ 
        [ Fσ.patternContinue σ scheme target
        | scheme <- schemes
        , target <- concretise τ
        ]



-- | Instantiate a rule. Instantiation entails the following:
--
-- 1. Sort the order of its premises in decreasing order of size. This makes
-- sure that the matching algorithm will work efficiently later on. (TODO)
-- 2. Generating the variable assignments of its generative constraints.
--
-- Note that a rule is useless if there is not a single appropriate assignment 
-- for the generator.
instantiateRule :: forall ext . (F.Extension ext)
                 => (StaticTerms ext -> [F.Term ext])
                 -> RuleUninstantiated ext
                 -> Maybe (RuleInstantiated ext)
instantiateRule concretise (n := ρ@Rule {generator}) = 
    fmap 
        (\generator' -> n := ρ { generator = generator' })
        (L.fromList . assign $ generator)

    where

    -- | Turn a specification of a generator into an actual set of possible
    -- assignments.
    assign :: Constraint PrimitiveStaticTerms ext -> [Fσ.Substitution ext]
    assign ι = nub $ case ι of
        None      -> [mempty]
        Choose ιs -> concat $ map assign ιs
        Merge  ιs -> merge  $ map assign ιs
        Match (F.Ambiguous schemes) terms -> catMaybes $ 
            [ Fσ.pattern scheme target 
            | scheme <- schemes
            , target <- concretise terms
            ]

    -- | Create a single generator that respects multiple generators 
    -- simultaneously. To see that this is valid, observe that a generator is 
    -- really just a list of possible assignments, e.g., a disjunction. To 
    -- rearrange a conjunction of disjunctions into a single disjunction, we
    -- find every way to draw a single disjunct from each conjunct, and merge
    -- every non-conflicting combination we thus find.
    merge :: [[Fσ.Substitution ext]] -> [Fσ.Substitution ext]
    merge = mapMaybe (foldM Fσ.merge mempty) . combinations


{-
-- | 'Free' or 'generative' variables are those variables that occur in the 
-- conclusion, but are not bound by the premise.
free :: F.Extension ext => RuleInstantiated ext -> [String]
free (_ := Rule {consumptions, productions}) = 
    nub (concat productions >>= F.variables) 
    \\  (consumptions >>= F.variables) 
-}


-- | A variation on permutations: given a list that describes the possible
-- elements at each position, give all possible element combinations. In a
-- sense, this is a @transpose@ operation.
--
-- Example: @[[1,2],[3,4]] -> [[1,3],[1,4],[2,3],[2,4]]@
combinations :: [[a]] -> [[a]]
combinations [] = []
combinations zs = foldr (\xs xss -> [ y:ys | y <- xs, ys <- xss ]) [[]] zs


