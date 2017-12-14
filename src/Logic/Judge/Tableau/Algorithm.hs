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
import "base" Data.List (intersect, partition, lookup, nub)
import "base" Data.Char (toLower)
import "base" Control.Applicative (Alternative, empty)
import "base" Control.Monad (foldM, guard, forM, join)
import qualified "containers" Data.Tree as R
import qualified "containers" Data.Map as M
import qualified "mtl" Control.Monad.State.Lazy as ST

import Logic.Judge.Printer ()
import Logic.Judge.Tableau.Specification (Ref((:=)))
import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Formula.Substitution as Fσ
import qualified Logic.Judge.Tableau.Specification as TS


-- | Formulas on the branch are decorated by their reference number and marks.
type BranchFormula ext = Ref Int (F.Marked (F.Formula ext))


-- | A proof in a tableau system is a rose tree, containing formulas and the
-- rule applications used to obtain them.
type Tableau ext = R.Tree (TableauNode ext)
data TableauNode ext
    = App (String, [Int]) [BranchFormula ext]
    | Root (BranchFormula ext)
    | Closure


-- | Read-only, static tableau "globals".
data TableauSettings ext = TableauSettings
    { rulesαβ :: [TS.RuleInstantiated ext]
    , root :: F.Marked (F.Formula ext)
    , assumptions :: [F.Formula ext]
    }


-- | A "working" branch keeps track of the leaf of a single branch and all 
-- that came before.
data Branch ext = Branch
 -- | Which ε-rules can still be used on the branch?
    { rulesε :: Maybe (L.PointedList (TS.RuleInstantiated ext))
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
 -- | Generator as it remains on the branch
    , generator' :: Maybe (L.PointedList (Fσ.Substitution ext))
 -- | Which rule was applied to find the match.
    , rule :: TS.RuleInstantiated ext
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
-- by the matched rule. (TODO: Confusing.)
concreteProductions :: (F.Extension ext, Monad m) 
                    => Match ext 
                    -> m [[F.Marked (F.Formula ext)]]
concreteProductions (Match {assignment, rule}) = 
    let mapM2 = mapM . mapM
    in Fσ.substitute assignment `mapM2` TS.productions rule



-- | Take the first option from a list of options.
greedy :: (Alternative f) => [a] -> f a
greedy []    = empty
greedy (x:_) = pure x



-- | A tableau rule has zero or more formula schemes that represent
-- consumptions. These must be matched against the concrete formulas that
-- remain on the branch unprocessed. This function provides all possible 
-- 'Match'es, but does not further instantiate the matches.
--
-- For efficiency, the 'biggest' formulas should be the first to be matched.
-- Note that the remainder is always focused on the element next to the last
-- removed element.
matchRule :: forall ext . (F.Extension ext)
          => Branch ext
          -> TS.RuleInstantiated ext
          -> [Match ext]
matchRule π ρ@TS.Rule {TS.consumptions, TS.generator} =
    foldM match μ₀ consumptions
    
    where

    -- | Initial match.
    μ₀ :: Match ext
    μ₀ = Match 
        { matched = mempty
        , assignment = mempty
        , remainder = actives π
        , rule = ρ
        , generator' = return generator
        }

    -- | Obtain all possibilities for matching one additional formula to an
    -- existing partial match.
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
   


-- | Greedily pick a rule and consumptions to work with. Obtain all possible
-- instantiations of said rule.
matches :: forall ext . (F.Extension ext)
        => TableauSettings ext 
        -> Branch ext 
        -> [Match ext]
matches κ@(TableauSettings {rulesαβ}) π = join $ do 
    μs <- instantiations
    let TS.Rule {TS.compositor} = rule $ head μs 
    case compositor of
        TS.Greedy -> greedy <$> return μs
        TS.Nondeterministic -> return μs

    where
    -- | Keep nondeterminism at the level of generated instances, by picking a 
    -- rule and its consumptions greedily, then instantiating said rule, moving
    -- on to the next match only if there turn out to be no applicable 
    -- instantiations for the current one.
    instantiations :: (Monad m, Alternative m) => m [Match ext]
    instantiations = greedy . filter (not . null) . map instantiate 
                   $ rulesαβ >>= matchRule π


    -- | Generate instances of a rule that has been partially matched.
    instantiate :: Match ext 
                -> [Match ext]
    instantiate μ@(Match {assignment, generator', rule}) = do
        σF <- maybe [] L.focus generator'
        σ <- Fσ.merge assignment (L.current σF)
        let (ρ@TS.Rule {TS.constraint}) = rule
        guard (TS.respects (concretiser (dynamic κ π)) σ constraint)
        return μ 
            { assignment = σ
            , generator' = L.delete σF
            }



-- | Provide all possibilities for expanding a single branch once: greedily
-- selecting the rules and formulas to apply them to, but possibly keeping the
-- rule instance nondeterministic.
expand1 :: forall ext . (F.Extension ext)
        => TableauSettings ext
        -> Branch ext
        -> [[Branch ext]]
expand1 κ@(TableauSettings {rulesαβ, assumptions})
        π@(Branch {rulesε, inactives, counter}) 
        = optionsαβ {-<|> optionsε doesn't work this way... -}

    where

    optionsαβ :: [[Branch ext]]
    optionsαβ = do
        -- Greedily pick a rule and formulas on the branch, and, depending on
        -- the rule, nondeterministically pick an instance of that rule.
        μ@(Match {matched, remainder}) <- matches κ π
        -- Unwrap the productions of the match we picked
        disjunction <- concreteProductions μ
        -- Present the newly created branches
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
{-
    optionsε :: [[Branch ext]]
    optionsε = do
        -- Pick a rule to try
        ρF <- maybe [] L.focus rulesε
        let ρ = L.current ρF
        -- Obtain a match with the current branch
        μ@(Match {matched, assignment, rule, remainder, constraint}) <- 
            matches τ ρ π
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

-}



-- | Recursively 'expand' a branch and obtain the first closed subtableau that
-- can be constructed in this way.
subtableau :: forall ext . (F.Extension ext) 
           => TableauSettings ext
           -> Branch ext 
           -> Maybe (Tableau ext)
subtableau κ = greedy . subtableaux

    where 

    -- | Obtain all possible subtableaux under a branch.
    subtableaux :: Branch ext -> [Tableau ext]
    subtableaux π@(Branch {lastFormulas, lastMatch, actives}) 
      = case lastMatch of
            Nothing -> (R.Node . Root . L.current . fromJust $ actives) <$> expand π
            Just μ@(Match {matched, rule}) -> 
                let ref = (TS.name rule, map TS.reference matched) 
                in R.Node (App ref lastFormulas) <$> case closed π of
                    True  -> return [R.Node Closure []]
                    False -> expand π

    -- | Nondeterministically and recursively expand the given branch into its 
    -- subtableaux.
    expand :: Branch ext -> [[Tableau ext]]
    expand π = do
        -- Pick a possible set of branch expansions
        πs <- expand1 κ π
        -- Recursively expand those branch expansions, too
        mapM subtableaux πs



-- | A non-essential step: make the reference numbers on the formulas 
-- heterogeneous, even if they are on different branches. This is done in a 
-- single step at the end so that we don't have the mental (and computational) 
-- burden of carrying a State monad everywhere. 
renumber :: Tableau ext -> Tableau ext
renumber = flip ST.evalState (0,[]) . renumber'

    where
    -- The renumbering is done by keeping track of the number of times we
    -- traversed 'up' a branch. We also remember the translation table for the
    -- current branch. The latter could probably be done implicitly, but this
    -- is easier to grasp.

    renumber' :: Tableau ext -> ST.State (Int, [(Int,Int)]) (Tableau ext)
    renumber' (R.Node ν children) = do
        proof <- R.Node <$> node ν <*> mapM renumber' children
        ST.modify (\(δ,tt) -> (δ+1,tt)) 
        return proof
     
    node :: TableauNode ext -> ST.State (Int, [(Int,Int)]) (TableauNode ext)
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
-- successfully shown the formula's validity and can return a 'Tableau'.
decide :: forall ext . (F.Extension ext) 
       => TS.TableauSystem ext
       -> F.Formula ext
       -> Maybe (Tableau ext)
decide system goal = renumber <$> uncurry subtableau (initial system goal)


-- | Construct the initial branch and settings for the decision algorithm.
initial :: forall ext . (F.Extension ext) 
        => TS.TableauSystem ext
        -> F.Formula ext
        -> (TableauSettings ext, Branch ext)
initial system goal = (initκ, initπ)

    where

    -- Initial settings
    initκ :: TableauSettings ext
    initκ = TableauSettings
        { rulesαβ = rulesαβ
        , root = φ
        , assumptions = TS.assumptions system
        }

    -- Initial branch
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

    rootN :: Int
    rootN = 0

    -- | Root of the tableau.
    root :: L.PointedList (BranchFormula ext)
    root = L.singleton (rootN := φ)

    -- | Rules are seperated into ε-rules (which do not consume anything) and
    -- αβ-rules (which do). The former may be applied only once per branch,
    -- while the latter can be applied any number of times. 
    (rulesε, rulesαβ) = 
          partition (null . TS.consumptions) 
        . mapMaybe (TS.instantiateRule (concretiser (static initκ)))
        . TS.rules 
        $ system




-- | Generic concretiser to convert a specification of terms into concrete
-- terms.
concretiser :: forall ext primitive . (Eq ext, Fσ.Substitutable ext ext)
            => (primitive -> [F.Term ext])
            -> TS.Terms primitive ext
            -> [F.Term ext]
concretiser primitive τ = nub $ case τ of
    TS.Primitive τ      -> primitive τ
    TS.Transform _ f τ  -> f            $ concretiser primitive τ
    TS.Union τs         -> concat       $ map (concretiser primitive) τs
    TS.Intersection τs  -> intersection $ map (concretiser primitive) τs


-- | Convert a primitive static term specification into concrete terms.
static :: TableauSettings ext
       -> TS.PrimitiveStaticTerms
       -> [F.Term ext]
static κ@(TableauSettings {root, assumptions}) τ = case τ of
    TS.Root -> F.asTerm root
    TS.Assumption -> map F.Formula assumptions


-- | Convert a primitive dynamic term specification into concrete terms.
dynamic :: TableauSettings ext
        -> Branch ext
        -> TS.PrimitiveDynamicTerms
        -> [F.Term ext]
dynamic κ π τ = case τ of
    TS.Static τ'   -> static κ τ'
    TS.Unprocessed -> map TS.value (maybe [] L.asList $ actives π) 
                      >>= F.asTerm
    TS.Processed   -> map TS.value (inactives π) 
                      >>= F.asTerm


-- | Take the intersection of all given lists.
intersection :: (Eq a) => [[a]] -> [a]
intersection [] = []
intersection xs = foldr1 intersect xs

