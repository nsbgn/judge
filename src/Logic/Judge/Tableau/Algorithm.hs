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

import Logic.Judge.Tableau.Specification (Ref((:=)))
import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Formula.Substitution as Fσ
import qualified Logic.Judge.Tableau.Specification as TS


-- | Formulas on the branch are decorated by their reference number and marks.
type BranchFormula ext = Ref Int (F.Marked (F.Formula ext))


-- | Like 'Either', but remembers the original input in the Right case too.
data Result input output
    = Success input output
    | Failure input


-- | A proof in a tableau system is a rose tree, containing formulas and the
-- rule applications used to obtain them.
data Tableau ext
    = Node [BranchFormula ext] (Tableau ext)
    | Application String [Int] [Tableau ext]
    | Closure


-- | Read-only, static tableau "globals".
data TableauSettings ext = TableauSettings
    { rulesC :: [TS.RuleInstantiated ext]
    -- ^ The consumer rules are those rules that take a consumption from the
    -- branch. They are always available.
    , root :: F.Marked (F.Formula ext)
    -- ^ The root of the tableau.
    , assumptions :: [F.Formula ext]
    -- ^ The assumptions or the constant specification.
    }


-- | A "working" branch keeps track of the leaf of a single branch and all 
-- that came before.
data Branch ext = Branch
    { rulesA :: Maybe (L.PointedList (TS.RuleInstantiated ext))
    -- ^ The ascetic rules are those rules that do not take a consumption from
    -- the branch. In order to guarantee termination, they can be used only
    -- once per branch — we therefore keep track of which rules can still be
    -- used.
    , actives :: Maybe (L.PointedList (BranchFormula ext))
    -- ^ Which formulas still lie unprocessed on the branch?
    , inactives :: [BranchFormula ext]
    -- ^ Which formulas have been processed on the branch?
    , new :: [BranchFormula ext]
    -- ^ Which formulas were the last to be introduced?
    , counter :: Int
    -- ^ What is the next identifier never to have occurred on the branch before?
    }


-- | A 'Match' keeps track of information that is required when we are checking
-- the applicability of a rule to a branch.
data Match ext = Match
    { matched :: [BranchFormula ext]
    -- ^ Which formulas (including their IDs) have been matched?
    , assignment :: Fσ.Substitution ext
    -- ^ Variable assignments forced by the match so far.
    , remainder :: Maybe (L.PointedList (BranchFormula ext))
    -- ^ Formulas remaining active on the branch after this match.
    , rule :: TS.RuleInstantiated ext
    -- ^ Which rule was applied during the match?
    , rule' :: Maybe (TS.RuleInstantiated ext)
    -- ^ If application of the rule changes the rule, that is recorded here. 
    }




-- | Determine if adding some new formulas will close the branch by causing a
-- contradiction.
-- 
-- TODO: Note that some marks may denote that its formula is false. This is 
-- taken into account, but precisely *which* marks denote falsity is hardcoded 
-- for the moment.
--
-- TODO: Also say WHICH formulas contradict.
--
-- | Determine if the most recent additions to the branch cause the branch to
-- close by causing a contradiction.
--
--
--- To test if a branch is really closed, it is only necessary to check if the
-- *newly added* formulas conflict with the *remaining* formulas; it's not
-- necessary to check for conflict in all formulas. Since there will only be a
-- few formulas new, we will assume that the first argument is smaller than the
-- second.-
closes :: Eq ext 
       => TableauSettings ext 
       -> Branch ext 
       -> Bool
closes κ@(TableauSettings {assumptions}) 
       π@(Branch {new, actives, inactives}) =
   
    any (F.contradict new') [ new'
                            , fmap strip $ maybe [] L.asList actives
                            , assumptions
                            , fmap strip inactives ]
    
    where
    new' = fmap strip new

    strip :: BranchFormula ext -> F.Formula ext
    strip (_ := (F.Marked m x)) = 
        if any (== "F") m 
            then F.negation x 
            else x



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
matchRule π ρ@TS.Rule {TS.consumptions} =
    foldM match μ₀ consumptions
    
    where

    -- | Initial match.
    μ₀ :: Match ext
    μ₀ = Match 
        { matched = mempty
        , assignment = mempty
        , remainder = actives π
        , rule = ρ
        , rule' = Just ρ
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



-- | Generate a match for every instance of the rule that was partially
-- matched.
instantiateMatch :: forall ext . (F.Extension ext)
                 => TableauSettings ext
                 -> Branch ext
                 -> Match ext 
                 -> [Match ext]
instantiateMatch κ π
                 μ@(Match { assignment
                          , rule'=Just (ρ@TS.Rule { TS.constraint
                                                  , TS.generator })
                          }) = do
    σF <- L.focus generator
    σ <- Fσ.merge assignment (L.current σF)
    guard (TS.respects (concretiser (dynamic κ π)) σ constraint)
    return μ 
        { assignment = σ
        , rule' = fmap (\g -> ρ { TS.generator = g}) (L.delete σF)
        }



-- | Greedily pick a consumer rule and consumptions to work with, but obtain
-- all possible instantiations of said rule. This way, nondeterminism is kept
-- at the level of generated instances.
--
-- Note that consumer rules are always picked in the order that they are
-- specified in the tableau settings κ.
matchFirst :: forall ext . (F.Extension ext)
           => TableauSettings ext 
           -> Branch ext 
           -> [Match ext]
matchFirst κ@(TableauSettings {rulesC}) π = join $ do 
    μs <- greedy
        . filter (not . null) 
        . map (instantiateMatch κ π)
        $ rulesC >>= matchRule π
    case TS.compositor . rule . head $ μs of
        TS.Greedy -> greedy <$> return μs
        TS.Nondeterministic -> return μs



-- | Provide all possibilities for expanding a single branch once: greedily
-- selecting the rules and formulas to apply them to, but possibly keeping the
-- rule instance nondeterministic.
expand1 :: forall ext . (F.Extension ext)
        => TableauSettings ext
        -> Branch ext
        -> [(Match ext, [Branch ext])]
expand1 κ@(TableauSettings {rulesC, assumptions})
        π@(Branch {rulesA, inactives, counter}) =
        if null consumers
            then ascetics
            else consumers
            
    where

    -- | All instances of a greedily picked rule for expanding a branch using
    -- a consumer rule.
    consumers :: [(Match ext, [Branch ext])]
    consumers = do
        -- Greedily pick a rule and formulas on the branch, and, depending on
        -- the rule, nondeterministically pick an instance of that rule.
        μ@(Match {matched, remainder, assignment, rule}) <- matchFirst κ π
        -- Instantiate and unwrap the productions of the match we picked
        disjunction <- Fσ.substitute2 assignment (TS.productions rule)
        -- Present the newly created branches
        return $ (,) μ
            [ π { actives = L.insertAll conjunction remainder
                , inactives = matched ++ inactives
                , new = conjunction
                , counter = counter + length conjunction
                }
            | conjunction <- zipWith (:=) [counter..] <$> disjunction 
            ]
    
    -- | An ascetic rule must always be greedy.
    ascetics :: [(Match ext, [Branch ext])]
    ascetics = greedy $ do
        ρF <- maybe [] L.focus rulesA
        let ρ = L.current ρF
        μ@(Match {matched, remainder, assignment, rule, rule'}) <- 
            matchRule π ρ >>= instantiateMatch κ π
        disjunction <- Fσ.substitute2 assignment (TS.productions rule)
        return $ (,) μ
            [ π { actives = L.insertAll conjunction remainder
                , inactives = matched ++ inactives
                , new = conjunction
                , counter = counter + length conjunction
                , rulesA = L.update ρF rule'
                }
            | conjunction <- zipWith (:=) [counter..] <$> disjunction
            ]



-- | Recursively 'expand' a branch and obtain the first closed subtableau that
-- can be constructed in this way.
subtableau :: forall ext . (F.Extension ext) 
           => TableauSettings ext
           -> Branch ext 
           -> Maybe (Tableau ext)
subtableau κ = greedy . subtableaux

    where 

    -- | Nondeterministically and recursively expand the given branch into its 
    -- subtableaux.
    subtableaux :: Branch ext -> [Tableau ext]
    subtableaux π@(Branch {new}) = Node new <$> case closes κ π of
        True -> return Closure
        False -> do
            -- Pick a possible set of branch expansions
            (Match {matched, rule}, πs) <- expand1 κ π
            -- Determine which rule led to the expansions
            let name = TS.name rule 
            let refs = map TS.reference matched
            -- Recursively expand those branch expansions, too
            Application name refs <$> mapM subtableaux πs



-- | A non-essential post-processing step on the tableau: make the reference 
-- numbers on the formulas heterogeneous, even if they are on different 
-- branches. This is done in a single step at the end so that we don't have 
-- the mental (and computational) burden of carrying a State monad everywhere. 
renumber :: Tableau ext -> Tableau ext
renumber = flip ST.evalState (1, []) . renumber'

    where
    -- The renumbering is done by keeping track of the number of times we
    -- traversed 'up' a branch. We also remember the translation table for the
    -- current branch. The latter could probably be done implicitly, but this
    -- is easier to grasp.
    renumber' :: Tableau ext -> ST.State (Int, [(Int,Int)]) (Tableau ext)
    renumber' tableau = do
        θ' <- case tableau of
            Closure -> do
                ST.modify (\(δ, assoc) -> (δ-1, tail assoc))
                return tableau
            Node φs θ -> do
                φs' <- forM φs $ \(i := φ) -> do
                    (δ, assoc) <- ST.get
                    let j = i + δ
                    ST.put (δ, (i,j):assoc)
                    return $ j := φ
                Node φs' <$> renumber' θ
            Application name refs θs -> do
                (δ, assoc) <- ST.get
                refs' <- forM refs $ return . fromJust . flip lookup assoc
                Application name refs' <$> mapM renumber' θs
        ST.modify (\(δ, assoc) -> (δ+1, assoc))
        return θ'


-- | A non-essential post-processing step on the tableau: if the root formula 
-- is not exactly equal to the input formula, there was supposedly a rewriting
-- step. Add this step to the tableau explicitly, to show what happened.
rewritten :: F.Extension ext 
          => F.Formula ext 
          -> Tableau ext 
          -> Tableau ext
rewritten φ θ@(Node [i := F.Marked m ψ] _) = 
    if φ == ψ
        then θ
        else Node [subtract 1 i := F.Marked m φ] 
           $ Application "rewrite" [i-1] [θ]


-- | Decide the validity of the target formula within the given logical system.
-- A branch closes when it internally contradicts. A branch that is neither 
-- closed nor expandable corresponds to a satisfying assignment of the negation
-- of the target formula, and constitutes a counter-model. Otherwise, we have
-- successfully shown the formula's validity and can return a 'Tableau'.
decide :: forall ext . (F.Extension ext) 
       => TS.TableauSystem ext
       -> F.Formula ext
       -> Result (F.Formula ext) (Tableau ext)
decide system goal =
    let postprocess = renumber . rewritten goal
        result = uncurry subtableau (initial system goal)
    in  maybe (Failure goal) (Success goal) $ postprocess <$> result 





-- | Construct the initial branch and settings for the decision algorithm.
initial :: forall ext . (F.Extension ext) 
        => TS.TableauSystem ext
        -> F.Formula ext
        -> (TableauSettings ext, Branch ext)
initial system goal = (initκ, initπ)

    where

    -- | Initial settings
    initκ :: TableauSettings ext
    initκ = TableauSettings
        { rulesC = rulesC
        , root = TS.value $ L.current root
        , assumptions = TS.assumptions system
        }

    -- | Initial branch
    initπ :: Branch ext
    initπ = Branch 
        { actives = return root
        , inactives = []
        , rulesA = L.fromList rulesA
        , new = L.toList root
        , counter = 1
        }

    -- | Root of the tableau.
    root :: L.PointedList (BranchFormula ext)
    root = L.singleton (0 := F.Marked ["F"] (F.simplify goal))

    -- | Rules are seperated into ascetic rules and consumer rules. To preserve
    -- termination, the former may be applied only once per branch, while the
    -- latter can be applied any number of times.
    (rulesA, rulesC) = 
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

