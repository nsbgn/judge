-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Specification
Description : Preprocesses the input to the tableau algorithm.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , rules         :: [RulePlain ext]
    , assumptions   :: [F.Formula ext]
    }


-- | Relates values to their identifiers.
data Ref ref val = (:=) { reference :: ref, value :: val }
infixr 7 :=

-- | Relates 'premises', 'antecedents' or 'conditions' to
-- 'conclusions', 'consequents' or 'results'.
data BaseRule ext =  [F.Marked (F.Formula ext)]
                 :> [[F.Marked (F.Formula ext)]]
infixr 9 :>

-- | Relate a rule to its constraint.
data Guard rule constraint = rule :| (ConstraintHandler,constraint)
infixr 8 :|

-- | An uninstantiated rule.
type RulePlain ext = Ref String (Guard (BaseRule ext) (Constraint ext))

-- | An instantiated tableau rule.
type Rule ext = Ref String (Guard (BaseRule ext) (ConstraintX ext))

-- | Extract the premises from a rule.
premises :: Ref String (Guard (BaseRule ext) constraint) 
         -> [F.Marked (F.Formula ext)]
premises (_ := φ :> _ :| _) = φ

-- | Extract the conclusions from a rule.
conclusion :: Ref String (Guard (BaseRule ext) constraint) -> [[F.Marked (F.Formula ext)]]
conclusion (_ := _ :> ψ :| _) = ψ

-- | Extract the constraint from a rule.
constraint :: Ref String (Guard (BaseRule ext) constraint) -> constraint
constraint (_ := _ :> _ :| (_,ι)) = ι


-- | Represent sets of formulas to be used in constraints. Note that some
-- constructors correspond to "static" sources, whereas others correspond to
-- "dynamic" sources. Only static sources can be used to bind to generative
-- variables, since they have to be known from the start.
data TermsPrimitive
 -- | Static: Goal formula.
    = Root
 -- | Static: Assumption formulas or constant specification.
    | Assumption
 -- | Dynamic: 'Active' terms currently not processed on the branch. 
    | Unprocessed
 -- | Dynamic: 'Inactive' terms currently processed on the branch.
    | Processed


-- | Represent terms to use in constraints. Note that only static
-- terms can bind to generative variables.
data TermsSpecification ext
    = Primitive TermsPrimitive
 -- | Keep terms that occur in at least one constituent.
    | Union [TermsSpecification ext]
 -- | Keep only terms that occur in all constituents.
    | Intersection [TermsSpecification ext]
 -- | Apply a transformation to contextual terms.
    | Transform String ([F.Term ext] -> [F.Term ext]) (TermsSpecification ext)
 -- | Filter (but don't bind) terms satisfying some pattern. (TODO)
 -- | Filter (F.Pattern ext) (TermsSpecification ext)



-- | A concretisation turns a primitive term specification into its concrete
-- terms.
--
-- Note that terms that can be marked formulas should also be given as their
-- unmarked counterparts; for example, it is not immediately clear that 'root' 
-- refers to the root formula including or excluding marks.
type TermsConcretisation ext = TermsPrimitive -> Maybe [F.Term ext]


-- | Indicates how to handle multiple instantiations.
data ConstraintHandler = Greedy | Nondeterministic


-- | A constraint is placed on a tableau rule to restrict the values to which
-- its variables can be bound. This means that some applications of the rule
-- will be blocked; but also that any 'free' or 'generative' variables (that 
-- is, variables that occur in the rule's conclusion but not in its premise) 
-- can now be associated with a set of possible assignments, thereby making 
-- it possible to, essentially, generate a *choice* of multiple 
-- *instantiations* of a singular rule.
--
-- 'Bind' and 'occurs' are both 'match' operators that ensure that a given
-- pattern matches with one term from a given set of terms. When interpreting
-- them both permissively, they would behave identically; the same goes for
-- interpreting them both prohibitively. The difference emerges in hybrid
-- constraints, where 'occurs' operators are interpreted prohibitively and
-- 'bind' operators are interpreted prohibitively.
data Constraint ext
    = None
 -- | Bind variables from a pattern to all terms that match the pattern.
 -- (TODO: but ignore special variable _).
    | Bind (Pattern ext) (TermsSpecification ext)
 -- | Check if the given pattern occurs in the set of terms.
 -- (TODO: enforce that all variables except for the special variable _ have 
 -- to be bound)
    | Occurs (Pattern ext) (TermsSpecification ext)
 -- | Constraint holds if one of the subconstraints hold.
    | Choose [Constraint ext]
 -- | Constraint holds if all subconstraints hold.
    | Merge [Constraint ext]


-- | A 'generative' (or 'permissive') constraint represents a choice between 
-- possible variable assignments. 
--
-- This approach is necessary to be able to handle 'free' variables, since by
-- definition such variables do not have a pre-existing binding to inspect for
-- compliance to the relevant constraint. Secondly, this makes it possible to
-- keep track of which bindings have already been attempted in the course of
-- an algorithm, thus making sure that 
--
-- This is required for handling 'free' ('generative') variables,
-- since, firstly, such variables do not have a pre-existing binding to inspect 
-- for compliance to the relevant constraint; and secondly, because this makes 
-- it possible to keep track of which bindings have been attempted.
--
-- The limitation of permissive constraints is that they are unchanging; 
-- no variable may be bound to one of a dynamic set of terms.
type ConstraintG ext = L.PointedList (Fσ.Substitution ext)


-- | Although generative constraints *do* also restrict bound variables (by 
-- "brute force": a variable's previous binding will block all conflicting 
-- assignments in the G-constraint), it is more computationally efficient to 
-- simply check already known values for satisfaction directly.
--
-- The limitation of prohibitive constraints is that they cannot deal with
-- 'free' variables. 
--
-- TODO: Note that unbound variables are 'thrown away' if they are used in a
-- pattern; this will cause confusion, so we should require that all variables
-- except the special variable "_" are bound elsewhere.
type ConstraintD ext = Constraint ext


-- | For efficiency, we use a hybrid between permissive constraints and
-- prohibitive constraints, by giving a (disjunctive) set of alternative 
-- intersections between two types.
--
-- Note the duality here: an empty permissive constraint means that the
-- constraint will always fail, whereas an empty prohibitive constraint means
-- that the constraint will always succeed. 
--
-- Note also that a permissive constraint represents a disjunction.
type ConstraintX ext = L.PointedList (ConstraintG ext, ConstraintD ext)




-- | Check that a given variable assignment does not conflict with the given
-- prohibitive constraint. At this point, all concrete terms should be known.
--
-- TODO: Note that this definition does not make sure that each schematic
-- variable in the found patterns refers to the same concrete variable; the
-- result of the pattern is 'thrown away'. Prohibitive constraints should
-- therefore only ever mention already bound variables.
respects :: forall ext . (F.Extension ext)
         => TermsConcretisation ext
         -> Fσ.Substitution ext
         -> ConstraintD ext
         -> Bool
respects τbase σ ι = case ι of
    None                 -> True
    Choose constraints   -> any (respects τbase σ) constraints 
    Merge constraints    -> all (respects τbase σ) constraints
    Occurs pattern τspec -> anyMatching pattern $ concrete' τspec
    _                    -> error2

    where 

    concrete' :: TermsSpecification ext -> [F.Term ext]
    concrete' τspec = maybe error2 id $ concrete τbase τspec

    anyMatching :: F.Ambiguous (F.Term ext) -> [F.Term ext] -> Bool
    anyMatching (F.Ambiguous schemes) terms = not . null . catMaybes $ 
        [ Fσ.patternContinue σ scheme target
        | scheme <- schemes
        , target <- terms
        ]

    -- | These errors are not supposed to show up if called with the correct
    -- arguments.
    error1, error2 :: a
    error1 = error "Cannot check 'bind' constraint during runtime."
    error2 = error "Terms not concretised; term concretisation wrong."


-- | 'Free' or 'generative' variables are those variables that occur in the 
-- conclusion, but are not bound by the premise.
free :: F.Extension ext => Rule ext -> [String]
free (_ := premises :> conclusion :| _) = 
    nub (concat conclusion >>= F.variables) 
    \\  (premises >>= F.variables) 



-- | Turn a specification of a set of terms (cf. 'Terms') into its concrete
-- terms. If the specification draws from a set of terms that is unknown at the
-- time (@Nothing@), the concretisation fails. 
concrete :: forall ext . (Eq ext, Fσ.Substitutable ext ext)
         => TermsConcretisation ext
         -> TermsSpecification ext
         -> Maybe [F.Term ext]
concrete τbase τspec = case τspec of
    Primitive base      -> τbase base
    Union τspecs        -> nub . concat       <$> mapM (concrete τbase) τspecs
    Intersection τspecs -> nub . intersection <$> mapM (concrete τbase) τspecs
    Transform _ f τspec -> f <$> concrete τbase τspec



-- | Update a rule's constraint to the given constraint.
withRule :: Ref String (Guard (BaseRule ext) constraint) 
         -> constraint1
         -> Ref String (Guard (BaseRule ext) constraint1)
withRule (n := φ :> ψ :| (ιhandler, ι)) ι' = n := φ :> ψ :| (ιhandler, ι')






-- | Instantiate a rule. Instantiation entails the following:
--
-- 1. Sort the order of its premises in decreasing order of size. This makes
-- sure that the matching algorithm will work efficiently later on. (TODO)
-- 2. Generating the assignments of its generative constraints, while deferring
-- to 'runtime' the simple checks given by degenerative constraints.
--
-- Fails if (a) a generative constraint attempts to draw from a dynamic term
-- set, or (b) if there is not a single appropriate assignment for it.
--
-- TODO: (a) should be reported to the user, whereas (b) should fail silently 
-- (as it does now).
instantiate :: forall ext . (F.Extension ext)
            => TermsConcretisation ext
            -> RulePlain ext
            -> Maybe (Rule ext)
instantiate source ρ@(n := φ :> ψ :| (ιhandler,ι)) = 
    withRule ρ <$> (instantiateList ι >>= makeZipper)

    where

    -- | Turn list-based constraint into zipper-based 'PointedList' constraint.
    makeZipper :: [([Fσ.Substitution ext], Constraint ext)] 
               -> Maybe (ConstraintX ext)
    makeZipper xs = mapM (\(g, d) -> (,d) <$> L.fromList g) xs >>= L.fromList


    -- | Turn a constraint specification into a list-based intersectional 
    -- constraint. Be careful: whereas a @Nothing@ degenerative constraint 
    -- denotes unconditional success (that is, the absence of restrictions), a 
    -- @Nothing@ intersectional constraint means failure.
    instantiateList :: Constraint ext 
                 -> Maybe [([Fσ.Substitution ext], Constraint ext)]
    instantiateList κ = case κ of
        None -> return [([mempty], None)]
        Choose constraints -> concat <$> mapM instantiateList constraints
        Merge constraints  -> mergeX <$> mapM instantiateList constraints

        -- This should lead to an error if the pattern contains generative
        -- variables that are not bound elsewhere.
        Occurs pattern termspec -> 
            return [([mempty], Occurs pattern termspec)]

        -- This should lead to an error if the constraint draws from dynamic
        -- terms.
        Bind pattern@(F.Ambiguous schemes) termspec -> do
            terms <- nub <$> concrete source termspec
            return $
                [( nub . catMaybes $ 
                    [ Fσ.pattern scheme target 
                    | scheme <- schemes
                    , target <- terms
                    ]
                , None
                )]


    -- | Combine a set of intersective constraints in such a way that the 
    -- result constraint simultaneously respects all of them.
    -- 
    -- To see that this is valid, note that an intersective constraint is a
    -- pair set (itself representing the intersection of generative and 
    -- degenerative constraint) that is to be interpreted disjunctively. 
    -- Respecting a *set* of such constraints amounts to interpreting them 
    -- conjunctively. We rearrange this conjunction back into a disjunction of 
    -- every non-conflicting combination of a single disjunct drawn from each 
    -- conjunct.
    mergeX :: [[([Fσ.Substitution ext], Constraint ext)]] 
           ->  [([Fσ.Substitution ext], Constraint ext)]
    mergeX = mapMaybe (merge2 . unzip) . combinations


    -- | Interpret two sets of constraints conjunctively.
    merge2 :: ([[Fσ.Substitution ext]], [Constraint ext])
           -> Maybe ([Fσ.Substitution ext], Constraint ext)
    merge2 (gs, ds) = 
        let (g, d) = (mergeG gs, mergeD ds)
        in  guard (not $ null g) >> return (g, d)


    -- | Interpret a set of generative constraints as a conjunction. The same
    -- reasoning as for 'mergeX' applies: to rearrange a conjunction into a
    -- disjunction, we group disjuncts from each conjunct together and merge 
    -- them appropriately.
    mergeG :: [[Fσ.Substitution ext]] -> [Fσ.Substitution ext]
    mergeG = nub . mapMaybe (foldM Fσ.merge mempty) . combinations


    -- | Interpret a set of degenerative constraints conjunctively.
    mergeD :: [Constraint ext] -> Constraint ext
    mergeD = 
        (\xs -> if null xs then None else Merge xs) 
        . filter (\x -> case x of {None -> False; _ -> True})




-- | A variation on permutations: given a list that describes the possible
-- elements at each position, give all possible element combinations. In a
-- sense, this is a @transpose@ operation.
--
-- Example: @[[1,2],[3,4]] -> [[1,3],[1,4],[2,3],[2,4]]@
combinations :: [[a]] -> [[a]]
combinations [] = []
combinations zs = foldr (\xs xss -> [ y:ys | y <- xs, ys <- xss ]) [[]] zs



-- | Take the intersection of all given lists.
intersection :: (Eq a) => [[a]] -> [a]
intersection [] = []
intersection xs = foldr1 intersect xs
