{-|
Module      : Logic.Judge.Prover.Tableau
Description : A tableau-based decision algorithm.
Copyright   : (c) 2017, 2018 N Steenbergen
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

A generic decision algorithm based on the method of analytic tableaux.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Prover.Tableau (
    -- * Input structures
      TableauSystem(..)
    , Constraint(..)
    , Compositor(..)
    , Rule(..)
    , RuleUninstantiated
    -- ** Term specification
    , PrimitiveDynamicTerms(..)
    , PrimitiveStaticTerms(..)
    , Terms(..)
    , DynamicTerms
    , StaticTerms
    -- * Decision algorithm
    , decide
    , initial
    -- ** Intermediate structures
    , TableauSettings(..)
    , Ref(..)
    , Branch(..)
    , BranchFormula
    , RuleInstantiated
    -- ** Output structures
    , Result(..)
    , Tableau(..)
    -- ** Postprocessors
    , shorten
    , renumber
    , rewrite
    -- * Auxiliaries
    , greedy
    , intersection
    , combinations
    ) where

import "base" Debug.Trace (trace, traceShow, traceM, traceShowM)

import "base" Data.Maybe (listToMaybe, mapMaybe, catMaybes, fromJust)
import "base" Data.List (intersect, partition, lookup, nub, sortBy, (\\))
import "base" Data.Function (on)
import "base" Control.Applicative (Alternative, empty, (<|>))
import "base" Control.Monad (foldM, guard, forM, forM_, join)
import qualified "containers" Data.Tree as R
import qualified "containers" Data.Map as M
import qualified "containers" Data.Set as S
import qualified "transformers" Control.Monad.Trans.State.Lazy as ST

import qualified Logic.Judge.PointedList as L
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Formula.Substitution as Fσ


-- | Formulas on the branch are decorated by their reference number and marks.
type BranchFormula ext = Ref Int (F.Marked (F.Formula ext))


-- | Relates values to their identifiers.
data Ref ref val = (:=) { reference :: ref, value :: val }
infixr 7 :=



-- | Like 'Either', but remembers the original input in the 'Right' case, too.
data Result input output
    = Success input output
    | Failure input


 
-- | A proof in a tableau system is a rose tree, containing sets of formulas
-- and the rule applications used to obtain them.
data Tableau ext
    = Node [BranchFormula ext] (Tableau ext)
    | Application String [Int] [Tableau ext]
    | Closure [Int]



-- | Before initialisation, the tableau system is read into this structure.
data TableauSystem ext = TableauSystem
    { title :: String
    , rules :: [RuleUninstantiated ext]
    , assumptions' :: [F.Formula ext]
    }



-- | The global state of the tableau — settings that will remain static after 
-- initialisation.
data TableauSettings ext = TableauSettings
    { rulesC :: [RuleInstantiated ext]
    -- ^ The consumer rules are those rules that take a consumption from the
    -- branch. They are always available.
    , root :: F.Marked (F.Formula ext)
    -- ^ The root of the tableau.
    , assumptions :: [F.Formula ext]
    -- ^ The assumptions or the constant specification.
    }


-- | A constraint is placed on a tableau rule to restrict the values to which
-- its variables can be bound. This means that some applications of the rule
-- will be blocked; but also that any "free" or "generative" variables (that 
-- is, variables that occur in the rule's productions but not in its
-- consumptions) can now be associated with a set of possible assignments, 
-- thereby making it possible to, essentially, generate a /choice/ of multiple 
-- /instantiations/ of a single rule.
data Constraint primitive ext
    = None
 -- | Demand that the pattern occurs in a particular set of terms.
    | Bind (F.Ambiguous (F.Term ext)) (Terms primitive ext)
 -- | Constraint holds if one of the subconstraints hold.
    | Choose [Constraint primitive ext]
 -- | Constraint holds if all subconstraints hold.
    | Merge [Constraint primitive ext]


-- | Indicates how to handle the situation where multiple rule instantiations
-- are applicable to the same formula.
--
-- Due to their computational complexity, rules that do not take any 
-- consumptions are handled greedily regardless of the value of the 
-- compositor.
data Compositor = Greedy | Nondeterministic


-- | Before instantiation, a generator is /described/ by a constraint. This
-- constraint can only refer to static terms.
type RuleUninstantiated ext = Rule (Constraint PrimitiveStaticTerms ext) ext


-- | After instantiation, a generator consists of all variable assignments that
-- it allows.
type RuleInstantiated ext = Rule (L.PointedList (Fσ.Substitution ext)) ext


-- | A rule describes which formulas it consumes and which it produces. In its
-- basic form, it can represent both instantiated and uninstantiated tableau
-- rules (see 'RuleInstantiated' and 'RuleUninstantiated').
data Rule generator ext = Rule 
    { name :: String
    -- ^ Identifier by which the rule shall be known.
    , consumptions :: [ F.Marked (F.Formula ext) ]
    -- ^ The consumptions (also: premises, antecedents, conditions) are
    -- formulas that are to be present on the branch before the rule may be
    -- applied.
    , productions :: [[ F.Marked (F.Formula ext) ]]
    -- ^ The productions (also: conclusions, consequents, results) are the
    -- formulas that will be created on the branch when the rule is applied. 
    -- Represents a disjunction of conjunctions.
    , generator :: generator
    -- ^ A generator is a "permissive constraint", which represents a choice 
    -- between possible variable assignments. This approach is necessary to be
    -- able to handle free variables in the productions: such variables
    -- do not have a pre-existing binding to check for compliance, so they 
    -- need to be created. This also makes it possible to keep track of which 
    -- bindings have already been attempted over the course of an algorithm,
    -- thus allowing for termination guarantees in case termination is not
    -- certain otherwise.
    -- 
    -- The limitation of the generator is that it is not very efficient and
    -- that no variable may be bound to terms from a dynamic set, since the 
    -- generator has to generate its instances at the beginning of the 
    -- algorithm. (Note that the last point can be dropped if we do not need to
    -- keep track of which bindings have already been used.)
    , constraint :: Constraint PrimitiveDynamicTerms ext
    -- ^ Although the generator /does/ also restrict bound variables (with 
    -- brute force: a variable's previous binding will block all conflicting 
    -- assignments), it is more computationally efficient to simply check 
    -- already known values for compliance, during runtime. 
    --
    -- The limitation of restrictive constraints is that they cannot deal with
    -- /free/ variables. 
    , compositor :: Compositor
    -- ^ The compositor indicates how to handle the case where multiple 
    -- instances are suggested by the generator.
    }


-- | A @Branch@ keeps track of the leaf of a single branch of the tableau, and
-- all that came before.
data Branch ext = Branch
    { rulesA :: Maybe (L.PointedList (RuleInstantiated ext))
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
    -- ^ The next identifier never to have occurred on the branch before.
    }


-- | A @Match@ keeps track of information that is required when we are checking
-- the applicability of a rule to a branch.
data Match ext = Match
    { matched :: [BranchFormula ext]
    -- ^ Which formulas (including their IDs) have been matched?
    , assignment :: Fσ.Substitution ext
    -- ^ Variable assignments forced by the match so far.
    , remainder :: Maybe (L.PointedList (BranchFormula ext))
    -- ^ Formulas remaining active on the branch after this match.
    , rule :: RuleInstantiated ext
    -- ^ Which rule was applied during the match?
    , rule' :: Maybe (RuleInstantiated ext)
    -- ^ If application of the rule changes the rule, that is recorded here. 
    }



-- | Determine if the most recent additions to the branch cause the branch to
-- close by causing a contradiction.
closes :: forall ext . Eq ext 
       => Branch ext 
       -> Maybe [Int]
closes π@(Branch {new, actives, inactives}) =

    first (map contradiction new) <|>
    contradict new new <|>
    contradict new inactives <|>
    contradict new (maybe [] L.asList actives)
  
    where

    first :: Foldable t => t (Maybe a) -> Maybe a
    first = foldl (<|>) Nothing

    contradict :: [BranchFormula ext] -> [BranchFormula ext] -> Maybe [Int]
    contradict xs ys = first (xs >>= \x -> return $ first (map (contradicts x) ys))

    hasF = any (=="F")

    contradiction :: BranchFormula ext -> Maybe [Int]
    contradiction (i := (F.Marked marks formula)) = case formula of
        F.Constant True -> if hasF marks 
            then Just [i] 
            else Nothing
        F.Constant False -> if not $ hasF marks 
            then Just [i] 
            else Nothing
        _ -> Nothing

    contradicts :: BranchFormula ext -> BranchFormula ext -> Maybe [Int]
    contradicts (i := F.Marked m1 f1) (j := F.Marked m2 f2) = 
        if ((hasF m1 && not (hasF m2)) || (hasF m2 && not (hasF m1))) && f1 == f2
            then Just [i, j]
            else Nothing



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
    Bind scheme τ  -> anyMatching scheme τ

    where 
    anyMatching :: F.Ambiguous (F.Term ext) -> DynamicTerms ext -> Bool
    anyMatching (F.Ambiguous schemes) τ = not . null . catMaybes $ 
        [ Fσ.patternContinue σ scheme target
        | scheme <- schemes
        , target <- concretise τ
        ]




-------------------------------------------------------------------------------
-- * Initialisation


-- | Instantiate a rule. Instantiation entails the following:
--
-- 1. Sort the order of its premises in decreasing order of size. 
-- 2. Generating the variable assignments as specified by the generator.
--
-- Note that a rule is useless if there is not a single appropriate assignment 
-- for the generator.
instantiateRule :: forall ext . (F.Extension ext)
                => (StaticTerms ext -> [F.Term ext])
                -> RuleUninstantiated ext
                -> Maybe (RuleInstantiated ext)
instantiateRule concretise ρ@Rule {generator, consumptions} = 
    fmap 
        (\instances -> ρ { generator = instances 
                         , consumptions = ordered consumptions })
        (L.fromList . assign $ generator)

    where

    -- | The consumptions should be sorted in order of decreasing complexity,
    -- since matching the most complex formula first (in `matchRule`) decreases
    -- the number of subsequent matches the most and will thus be more
    -- efficient.
    ordered :: [F.Marked (F.Formula ext)] -> [F.Marked (F.Formula ext)]
    ordered = reverse . sortBy (compare `on` F.size)

    -- | Turn a specification of a generator into an actual set of possible
    -- assignments.
    assign :: Constraint PrimitiveStaticTerms ext -> [Fσ.Substitution ext]
    assign ι = nub $ case ι of
        None      -> [mempty]
        Choose ιs -> concat $ map assign ιs
        Merge  ιs -> merge  $ map assign ιs
        Bind (F.Ambiguous schemes) terms -> catMaybes $ 
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





-- | Construct the initial branch and settings for the decision algorithm.
initial :: forall ext . (F.Extension ext) 
        => TableauSystem ext
        -> F.Formula ext
        -> (TableauSettings ext, Branch ext)
initial system goal = (initκ, initπ)

    where

    -- | Initial settings
    initκ :: TableauSettings ext
    initκ = TableauSettings
        { rulesC = rulesC
        , root = value $ L.current root
        , assumptions = assumptions' system
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
          partition (null . consumptions) 
        . mapMaybe (instantiateRule (concretiser (static initκ)))
        . rules 
        $ system



-------------------------------------------------------------------------------
-- * Matching branches

-- | A tableau rule has zero or more formula schemes that represent
-- consumptions. These must be matched against the concrete formulas that
-- remain on the branch unprocessed. This function provides all possible 
-- 'Match'es, but does not further instantiate the matches.
--
-- For efficiency, the "biggest" consumptions should be the first to be 
-- matched.
matchRule :: forall ext . (F.Extension ext)
          => Branch ext
          -> RuleInstantiated ext
          -> [Match ext]
matchRule π ρ@Rule {consumptions} =
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
                          , rule'=Just (ρ@Rule { constraint
                                                  , generator })
                          }) = do
    σF <- L.focus generator
    σ <- Fσ.merge assignment (L.current σF)
    guard (respects (concretiser (dynamic κ π)) σ constraint)
    return μ 
        { assignment = σ
        , rule' = fmap (\g -> ρ { generator = g}) (L.delete σF)
        }



-- | Greedily pick a consumer rule and consumptions to work with, but obtain
-- all possible instantiations of said rule. This way, nondeterminism is kept
-- at the level of generated instances.
--
-- Note that consumer rules are always picked in the order that they are
-- specified in the 'TableauSettings'.
matchFirst :: forall ext . (F.Extension ext)
           => TableauSettings ext 
           -> Branch ext 
           -> [Match ext]
matchFirst κ@(TableauSettings {rulesC}) π = join $ do 
    μs <- greedy
        . filter (not . null) 
        . map (instantiateMatch κ π)
        $ rulesC >>= matchRule π
    case compositor . rule . head $ μs of
        Greedy -> greedy <$> return μs
        Nondeterministic -> return μs



-------------------------------------------------------------------------------
-- * Decision algorithm


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

    mapM2 :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
    mapM2 = mapM . mapM

    -- | All instances of a greedily picked rule for expanding a branch using
    -- a consumer rule.
    consumers :: [(Match ext, [Branch ext])]
    consumers = do
        -- Greedily pick a rule and formulas on the branch, and, depending on
        -- the rule, nondeterministically pick an instance of that rule.
        μ@(Match {matched, remainder, assignment, rule}) <- matchFirst κ π
        -- Instantiate and unwrap the productions of the match we picked
        disjunction <- Fσ.substitute assignment `mapM2` productions rule
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
        disjunction <- Fσ.substitute assignment `mapM2` productions rule
        return $ (,) μ
            [ π { actives = L.insertAll conjunction remainder
                , inactives = matched ++ inactives
                , new = conjunction
                , counter = counter + length conjunction
                , rulesA = L.update ρF rule'
                }
            | conjunction <- zipWith (:=) [counter..] <$> disjunction
            ]



-- | Recursively expand a branch and obtain the first closed tableau that
-- can be constructed in this way.
expand :: forall ext . (F.Extension ext) 
       => TableauSettings ext
       -> Branch ext 
       -> Maybe (Tableau ext)
expand κ = greedy . expand'

    where 

    -- | Nondeterministically and recursively expand the given branch into its 
    -- subtableaux.
    expand' :: Branch ext -> [Tableau ext]
    expand' π@(Branch {new}) = Node new <$> case closes π of
        Just xs -> return $ Closure xs
        Nothing -> do
            -- Pick a possible set of branch expansions
            (Match {matched, rule}, πs) <- expand1 κ π
            -- Determine which rule led to the expansions
            let n = name rule 
            let refs = map reference matched
            -- Recursively expand those branch expansions, too
            Application n refs <$> mapM expand' πs



-- | Decide the validity of the target formula within the given logical system.
-- A branch closes when it internally contradicts. A branch that is neither 
-- closed nor expandable corresponds to a satisfying assignment of the negation
-- of the target formula, and constitutes a counter-model. Otherwise, we have
-- successfully shown the formula's validity and can return a 'Tableau'.
decide :: forall ext . (F.Extension ext) 
       => TableauSystem ext
       -> F.Formula ext
       -> Result (F.Formula ext) (Tableau ext)
decide system goal =
    let postprocess = renumber 1 . rewrite goal . shorten
        result = uncurry expand (initial system goal)
    in  maybe (Failure goal) (Success goal) $ postprocess <$> result 



-------------------------------------------------------------------------------
-- * Post-processing


-- | Eliminate rule applications that do not produce any formulas that are 
-- involved in closing any branch. 
--
-- Note that this will not eliminate all unnecessary applications (let alone
-- find the shortest proof) — it will only remove rules that are not involved
-- in any closure. For example, for justification logic, if @c:φ@ and @d:ψ@ 
-- are in the CS but only @d:ψ@ has to be introduced via CSr, then this will
-- remove any redundant CSr application — but if a formula is introduced via
-- a restricted cut, it could do nothing because the cut-formula IS involved in
-- the closure of a branch, even though it was pointless to do the cut in the
-- first place. It would be nice to think of a stronger method.
shorten :: Tableau ext 
        -> Tableau ext
shorten = flip ST.evalState S.empty . shorten'

    where
    -- | The boolean keeps track of whether the application above produced
    -- anything helpful; the set keeps track of relevant formula references.
    -- It is a bit opaque and not as succinct as I think it could be --- a
    -- rewrite is welcome.
    shorten' :: Tableau ext -> ST.State (S.Set Int) (Tableau ext)
    shorten' tableau = case tableau of
        Closure refs -> do
            ST.modify $ \s -> foldr S.insert s refs
            return tableau
        Application name refs θs -> do
            (relevances, θs') <- fmap unzip . forM θs $ \(Node φs θ) -> do
                θ' <- shorten' θ
                relevantRefs <- ST.get 
                relevant <- forM φs $ \(i := _) -> do
                    if S.member i relevantRefs
                        then ST.modify (S.delete i) >> return True
                        else return False
                return (or relevant, Node φs θ')
            if or relevances
            then do
                ST.modify (\s -> foldr S.insert s refs) 
                return $ Application name refs θs'
            else case θs' of
                (Node φs θ'' : _) -> return θ''
                _ -> error "This should not be possible."
        Node φs θ -> Node φs <$> shorten' θ



-- | Make the reference numbers on the formulas heterogeneous, even if they 
-- are on different branches. This is done in a single step at the end so that 
-- we do not have the mental (and computational) burden of carrying a 
-- 'ST.State' monad everywhere. 
renumber :: Int 
         -> Tableau ext 
         -> Tableau ext
renumber start = flip ST.evalState (start, []) . renumber'

    where
    -- The renumbering is done by keeping track of the number of times we
    -- traversed up a branch. We also remember the translation table for the
    -- current branch. The latter could probably be done implicitly, but this
    -- is easier to grasp.
    renumber' :: Tableau ext -> ST.State (Int, [(Int,Int)]) (Tableau ext)
    renumber' tableau = do
        θ' <- case tableau of
            Closure refs -> do
                (_, assoc) <- ST.get
                refs' <- forM refs $ return . fromJust . flip lookup assoc
                return $ Closure refs'
            Node φs θ -> do
                φs' <- forM φs $ \(i := φ) -> do
                    (j, assoc) <- ST.get
                    ST.put (j+1, (i,j):assoc)
                    return $ j := φ
                ν <- Node φs' <$> renumber' θ
                ST.modify (fmap . drop $ length φs')
                return ν
            Application name refs θs -> do
                (_, assoc) <- ST.get
                refs' <- forM refs $ return . fromJust . flip lookup assoc
                Application name refs' <$> mapM renumber' θs
        return θ'



-- | If the root formula is not exactly equal to the input formula, there was 
-- supposedly a rewriting step. Add this step to the tableau explicitly, to 
-- show what happened.
rewrite :: F.Extension ext 
        => F.Formula ext 
        -> Tableau ext 
        -> Tableau ext
rewrite φ θ@(Node [i := F.Marked m ψ] _) = 
    if φ == ψ
        then θ
        else Node [subtract 1 i := F.Marked m φ] 
           $ Application "rewrite" [i-1] [θ]



-------------------------------------------------------------------------------
-- * Term specifications

-- | Represent sets of primitive source formulas to be used in restrictive 
-- constraints. 
data PrimitiveDynamicTerms 
    = Static PrimitiveStaticTerms
 -- | Active terms, currently not processed on the branch. 
    | Processed
 -- | Inactive terms, currently processed on the branch.
    | Unprocessed


-- | Represent sets of primitive source formulas to be used in generators
-- and restrictive constraints. 
data PrimitiveStaticTerms 
 -- | Goal formula.
    = Root
 -- | Assumption formulas or constant specification.
    | Assumption


-- | Represent complex sets of source terms, to be turned into concrete terms
-- at a point where it is known what they should refer to. Static terms are 
-- known at the start of the tableau procedure, whereas dynamic terms should
-- be evaluated dynamically.
data Terms primitive ext
    = Primitive primitive
 -- | Keep terms that occur in at least one constituent.
    | Union [Terms primitive ext]
 -- | Keep only terms that occur in all constituents.
    | Intersection [Terms primitive ext]
 -- | Apply a transformation to terms.
    | Transform String ([F.Term ext] -> [F.Term ext]) (Terms primitive ext)


-- | Shorthand for a specification of complex dynamic terms.
type DynamicTerms = Terms PrimitiveDynamicTerms


-- | Shorthand for a specification of complex static terms.
type StaticTerms = Terms PrimitiveStaticTerms


-- | Generic concretiser to convert a specification of terms into concrete
-- terms.
concretiser :: forall ext primitive . (Eq ext, Fσ.Substitutable ext ext)
            => (primitive -> [F.Term ext])
            -> Terms primitive ext
            -> [F.Term ext]
concretiser primitive τ = nub $ case τ of
    Primitive τ      -> primitive τ
    Transform _ f τ  -> f            $ concretiser primitive τ
    Union τs         -> concat       $ map (concretiser primitive) τs
    Intersection τs  -> intersection $ map (concretiser primitive) τs


-- | Convert a primitive static term specification into concrete terms.
static :: TableauSettings ext
       -> PrimitiveStaticTerms
       -> [F.Term ext]
static κ@(TableauSettings {root, assumptions}) τ = case τ of
    Root -> F.asTerm root
    Assumption -> map F.Formula assumptions


-- | Convert a primitive dynamic term specification into concrete terms.
dynamic :: TableauSettings ext
        -> Branch ext
        -> PrimitiveDynamicTerms
        -> [F.Term ext]
dynamic κ π τ = case τ of
    Static τ'   -> static κ τ'
    Unprocessed -> map value (maybe [] L.asList $ actives π) 
                      >>= F.asTerm
    Processed   -> map value (inactives π) 
                      >>= F.asTerm



-------------------------------------------------------------------------------
-- * Auxiliaries

-- | Take the first option from a list of options.
greedy :: (Alternative f) => [a] -> f a
greedy []    = empty
greedy (x:_) = pure x


-- | Take the intersection of all given lists.
intersection :: (Eq a) => [[a]] -> [a]
intersection [] = []
intersection xs = foldr1 intersect xs


-- | A variation on permutations: given a list that describes the possible
-- elements at each position, give all possible element combinations. In a
-- sense, this is a @transpose@ operation.
--
-- Example: @[[1,2],[3,4]] -> [[1,3],[1,4],[2,3],[2,4]]@
combinations :: [[a]] -> [[a]]
combinations [] = []
combinations zs = foldr (\xs xss -> [ y:ys | y <- xs, ys <- xss ]) [[]] zs

