-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Algorithm
Description : A tableau-based decision algorithm.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Tableau.Algorithm where

import "base" Debug.Trace (trace, traceShow, traceM, traceShowM)

import "base" Data.Maybe (listToMaybe, mapMaybe, fromJust)
import "base" Data.Either (isRight)
import "base" Data.List (intersect, partition, lookup)
import "base" Data.Char (toLower)
import "base" Control.Monad (foldM, guard, forM)
import qualified "containers" Data.Tree as R
import qualified "containers" Data.Map as M
import qualified "mtl" Control.Monad.State.Lazy as ST

import Logic.Judge.Printer ()
import Logic.Judge.Tableau.Specification (Ref((:=)), Guard((:|)), BaseRule((:>)))
import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Formula.Substitution as Fσ
import qualified Logic.Judge.Tableau.Specification as TS


-- | Formulas on the branch are decorated by their reference number and marks.
type BranchFormula ext = Ref Int (F.Marked (F.Formula ext))


-- | A proof in a tableau system is a rose tree, containing formulas and the
-- rule applications used to obtain them.
type Proof ext = R.Tree (ProofNode ext)
data ProofNode ext
    = App (String, [Int]) [BranchFormula ext]
    | Root (BranchFormula ext)
    | Closure


-- | "Global" tableau state.
data Tableau ext = Tableau
    { rulesαβ :: [TS.Rule ext]
    , assumptions :: [F.Formula ext]
    , staticConcretisation :: TS.TermsConcretisation ext
    }


-- | A "working" branch keeps track of the leaf of a single branch and all 
-- that came before.
data Branch ext = Branch
 -- | Which ε-rules can still be used on the branch?
    { rulesε :: Maybe (L.PointedList (TS.Rule ext))
 -- | Which formulas still lie unprocessed on the branch?
    , actives :: Maybe (L.PointedList (BranchFormula ext))
 -- | Which formulas have been processed on the branch?
    , inactives :: [BranchFormula ext]
 -- | Is this branch closed?
    , closed :: Bool
 -- | What was the last match to be applied to this branch?
    , lastMatch :: Maybe (Match ext)
 -- | Which formulas were the last to be introduced?
    , lastFormulas :: [BranchFormula ext]
 -- | What is the next identifier never to have occurred on the branch before?
    , counter :: Int
    }


-- | A 'Match' keeps track of information that is required when we are checking
-- the applicability of a rule to a branch.
data Match ext = Match
 -- | Which formulas (including their IDs) have been matched.
    { matched :: [BranchFormula ext]
 -- | Variable assignments forced by the match.
    , assignment :: Fσ.Substitution ext
 -- | Formulas remaining active on the branch after this match.
    , remainder :: Maybe (L.PointedList (BranchFormula ext))
 -- | Constraint as it remains on the branch
    , constraint :: Maybe (TS.ConstraintX ext)
 -- | Which rule was applied to find the match.
    , rule :: TS.Rule ext
    }


-- | Obtain all formulas on the branch, both active and inactive.
formulas :: Branch ext -> [BranchFormula ext]
formulas π = maybe [] L.asList (actives π) ++ inactives π



-- | Determine if adding some new formulas will close the branch by causing a
-- contradiction.
-- 
-- TODO: Note that some marks may denote that its formula is false. This is 
-- taken into account, but precisely *which* marks denote falsity is hardcoded 
-- for the moment.
--
-- TODO: Strictly, a branch should only close when two *atomary* formulas
-- contradict.
--
-- TODO: Also say WHICH formulas contradict.
closes :: (Eq ext, Traversable t1, Traversable t2, Traversable t3) 
       => t1 (BranchFormula ext) 
       -> t2 (BranchFormula ext) 
       -> t3 (F.Formula ext) 
       -> Bool
closes new old assumptions = 
    fmap strip new `F.contradict` assumptions ||
    fmap strip new `F.contradict` fmap strip old

    where
    strip :: BranchFormula ext -> F.Formula ext
    strip (_ := (F.Marked m x)) = if any isNegator m 
        then F.negation x 
        else x

    isNegator :: String -> Bool
    isNegator = (`elem` ["-", "¬", "~", "×", "x", "false", "f"]) . map toLower
    


-- | Return a disjunction of conjunctions of formulas that would be introduced
-- by the matched rule.
introductions :: (F.Extension ext, Monad m) 
              => Match ext 
              -> m [[F.Marked (F.Formula ext)]]
introductions (Match {assignment, rule}) = 
    let mapM2 = mapM . mapM
    in  Fσ.substitute assignment `mapM2` TS.conclusion rule



-- | A tableau rule has zero or more formula schemes that represent premises.
-- Those premises must be matched against the concrete formulas that remain on
-- the branch unprocessed, and the additional constraints associated with the
-- rule. This function provides all possible 'Match'es for a single rule.
--
-- For efficiency, the 'biggest' formulas should be the first to be matched.
-- Note that the remainder is always focused on the element next to the last
-- removed element.
matches :: forall ext . (F.Extension ext)
        => TS.TermsConcretisation ext
        -> TS.Rule ext
        -> Branch ext
        -> [Match ext]
matches τ ρ@(name := premises :> conclusion :| (handler, constraint)) branch = 
    foldM match initial premises >>= constrain
    
    where

    -- | Initial match.
    initial :: Match ext
    initial = Match 
        { matched = mempty
        , assignment = mempty
        , remainder = actives branch
        , constraint = return constraint
        , rule = ρ
        }

    -- | Match one additional formula to an existing partial match.
    match :: Match ext -> F.Marked (F.Formula ext) -> [Match ext]
    match μ@(Match {matched, assignment, remainder}) scheme = do
        activeF <- maybe [] L.focus remainder
        let ν@(_ := formula) = L.current activeF
        σ <- Fσ.patternContinue assignment scheme formula
        return μ 
            { matched = ν : matched
            , remainder = L.delete activeF
            , assignment = σ
            }
    

    -- | Generate or prune matches from a partial match, based on constraints. 
    constrain :: Match ext -> [Match ext]
    constrain μ@(Match {assignment, constraint}) = do
        constraintF <- maybe [] L.focus constraint
        let (generative, degenerative) = L.current constraintF
        assignmentsF <- L.focus generative
        σ <- Fσ.merge assignment (L.current assignmentsF)
        guard (TS.respects τ σ degenerative)
        return μ 
            { assignment = σ
            , constraint = L.update constraintF $ 
                (, degenerative) <$> L.delete assignmentsF
            }



-- | Provide the first possibility for expanding a single branch once.
expand :: forall ext . (F.Extension ext)
       => Tableau ext
       -> Branch ext
       -> Maybe [Branch ext]
expand θ@(Tableau {rulesαβ, assumptions})
       π@(Branch {rulesε, inactives, counter}) 
       = listToMaybe $ optionsαβ ++ optionsε

    where
    τ :: TS.TermsConcretisation ext
    τ = dynamicConcretisation θ π

    optionsαβ :: [[Branch ext]]
    optionsαβ = do
        -- Pick a rule to try
        ρ <- rulesαβ
        -- Obtain a match with the current branch
        μ@(Match {matched, assignment, rule, remainder, constraint}) 
            <- matches τ ρ π 
        -- Obtain formulas matching
        disjunction <- introductions μ
        return $ 
            [ π { actives = L.insertAll conjunction remainder
                , inactives = matched ++ inactives
                , lastMatch = return μ
                , lastFormulas = conjunction
                , closed = closes conjunction (formulas π) assumptions
                , counter = counter + length conjunction
                }
            | conjunction <- zipWith (:=) [counter..] <$> disjunction
            ]

    optionsε :: [[Branch ext]]
    optionsε = do
        -- Pick a rule to try
        ρF <- maybe [] L.focus rulesε
        let ρ = L.current ρF
        -- Obtain a match with the current branch
        μ@(Match {matched, assignment, rule, remainder, constraint}) <- 
            matches τ ρ π
            --case handler of
            --    TS.Nondeterministic -> 
            --    TS.Greedy -> matches τ ρ π
        -- Obtain matching formulas
        disjunction <- introductions μ
        return $ 
            [ π { actives = L.insertAll conjunction remainder
                , inactives = matched ++ inactives
                , rulesε = L.update ρF $ TS.withRule ρ <$> constraint
                , lastMatch = return μ
                , lastFormulas = conjunction
                , closed = closes conjunction (formulas π) assumptions
                , counter = counter + length conjunction
                }
            | conjunction <- zipWith (:=) [counter..] <$> disjunction
            ]



-- | Use 'expand' to construct a 'Proof'.
construct :: forall ext . (F.Extension ext)
          => Tableau ext
          -> Branch ext 
          -> Either [BranchFormula ext] (Proof ext)
construct θ π@(Branch {lastFormulas, lastMatch, actives}) = case lastMatch of
    Nothing -> (R.Node . Root . L.current . fromJust $ actives) <$> subproof
    Just μ@(Match {matched, rule}) -> 
        let ref = (TS.reference rule, map TS.reference matched) 
        in R.Node (App ref lastFormulas) <$> case closed π of
            True  -> return [R.Node Closure []]
            False -> subproof

    where
    -- | Recursive call to obtain subproofs.
    subproof :: Either [BranchFormula ext] [Proof ext]
    subproof = maybe (Left $ formulas π) Right (expand θ π) 
           >>= mapM (construct θ)



-- | A non-essential step: make the reference numbers on the formulas 
-- heterogeneous, even if they are on different branches. This is done in a 
-- single step at the end so that we don't have the mental (and computational) 
-- burden of carrying a State monad everywhere. 
renumber :: Proof ext -> Proof ext
renumber = flip ST.evalState (0,[]) . renumber'

    where
    -- The renumbering is done by keeping track of the number of times we
    -- traversed 'up' a branch. We also remember the translation table for the
    -- current branch. The latter could probably be done implicitly, but this
    -- is easier to grasp.

    renumber' :: Proof ext -> ST.State (Int, [(Int,Int)]) (Proof ext)
    renumber' (R.Node ν children) = do
        proof <- R.Node <$> node ν <*> mapM renumber' children
        ST.modify (\(δ,tt) -> (δ+1,tt)) 
        return proof
     
    node :: ProofNode ext -> ST.State (Int, [(Int,Int)]) (ProofNode ext)
    node ν@(Root (i := _)) = ST.put (0,[(i,i)]) >> return ν
    node ν@(Closure) = ST.modify (\(δ,tt) -> (δ-1,tail tt)) >> return ν
    node ν@(App (r,ids) φs) = do
        (δ, tt) <- ST.get
        φs' <- forM φs $ \(i := φ) -> do
            let j = i + δ
            ST.modify (\(δ, tt) -> (δ, (i,j):tt))
            return $ j := φ
        ids' <- forM ids $ return . fromJust . flip lookup tt
        return $ App (r, ids') φs'



-- | Decide the validity of the target formula within the given logical system.
-- A branch closes when it internally contradicts. A branch that is neither 
-- closed nor expandable corresponds to a satisfying assignment of the negation
-- of the target formula, and constitutes a counter-model. Otherwise, we have
-- successfully shown the formula's validity and can return a 'Proof'.
decide :: forall ext . (F.Extension ext) 
       => TS.TableauSystem ext
       -> F.Formula ext
       -> Either [BranchFormula ext] (Proof ext)
decide system goal = renumber <$> uncurry construct (initial system goal)


-- | Construct the initial tableau and branch for the decision algorithm.
initial :: forall ext . (F.Extension ext) 
        => TS.TableauSystem ext
        -> F.Formula ext
        -> (Tableau ext, Branch ext)
initial system goal = (initθ, initπ)

    where
    rootN :: Int
    rootN = 0

    initθ :: Tableau ext
    initθ = Tableau
        { rulesαβ = rulesαβ
        , staticConcretisation = staticConcretisation
        , assumptions = TS.assumptions system
        }

    initπ :: Branch ext
    initπ = Branch 
        { actives = return root
        , inactives = []
        , rulesε = L.fromList rulesε
        , closed = closes root root (TS.assumptions system)
        , lastMatch = Nothing
        , lastFormulas = []
        , counter = rootN + 1
        }

    φ :: F.Marked (F.Formula ext)
    φ = F.Marked ["F"] (F.simplify goal)

    root :: L.PointedList (BranchFormula ext)
    root = L.singleton (rootN := φ)

    -- | Rules are seperated into ε-rules (which do not have any premises) and
    -- αβ-rules (which do). The former may be applied only once per branch,
    -- while the latter can be applied any number of times. 
    (rulesε, rulesαβ) = 
          partition (null . TS.premises) 
        . mapMaybe (TS.instantiate staticConcretisation) 
        . TS.rules 
        $ system

    staticConcretisation :: TS.TermsConcretisation ext
    staticConcretisation τ = case τ of
        TS.Root -> Just . F.asTerm $ φ
        TS.Assumption -> Just . map F.Formula $ TS.assumptions system
        _ -> Nothing



dynamicConcretisation :: Tableau ext -> Branch ext -> TS.TermsConcretisation ext
dynamicConcretisation θ π τ = case τ of
    TS.Unprocessed -> return $ (map TS.value . activesL  $ π) >>= F.asTerm
    TS.Processed   -> return $ (map TS.value . inactives $ π) >>= F.asTerm
    _ -> staticConcretisation θ τ

    where activesL = maybe [] L.asList . actives

