{-|
Module      : Logic.Judge.Formula.Substitution
Description : Obtain variable assignments and apply substitutions.
Copyright   : (c) 2017 ns@slak.ws
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

This module makes it possible to obtain variable assignments by comparing 
'Formula's, and to apply substitutions based on them.

The idea is similar to, but /not the same as/ unification. When we pattern @x@ 
to @y@, the former is schematic and the latter is literal. Even though their
variables can be structurally identical, they are really different, in that,
for example, @Var "x"@ may well pattern with @Implication (Var "x") (Var "x")@,
resulting in the substitution @[("x", Implication "x" "x")]@.

Nevertheless, see @subsumes@ from @Control.Unification@. They are not related 
by code, but the purpose is similar.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Formula.Substitution 
    ( Substitution
    , Substitutable(substitute, pattern, patternContinue)
    , merge
    ) where

import "base" Control.Monad (void, when, sequence, ap)
import "transformers" Control.Monad.Trans.Class (lift, MonadTrans)
import "transformers" Control.Monad.Trans.State.Lazy (StateT(StateT), execStateT, get, put, evalStateT)
import qualified "containers" Data.Map as M

import Logic.Judge.Formula.Datastructure


-- | A substitution maps identifiers to terms of a logic.
type Substitution ext = M.Map String (Term ext)


-- | Monad that supports state and failure.
type StateFail state result = StateT state Maybe result


-- | The 'Substitutable' class represents a relation between terms based on an 
-- extension @ext@ (that is, formulas or extensions of formulas) and subterms 
-- that may be substituted for variables inside those @ext@-terms.
class Substitutable ext term where

    -- | Apply a substitution to a term.
    --
    -- Note that at the time of writing, there is no fundamental distinction
    -- between variables and schematic variables.
    substitute :: Monad m 
               => Substitution ext 
               -> term
               -> m term


    -- | @a `pattern` b@ tries to find a substitution such that @a@ matches
    -- @b@. The @a@ formula is taken as a schematic formula, where variables
    -- represent gaps to be filled in, and @b@ is a "normal" formula, in which
    -- variables represent literals.
    --
    -- For simplicity, this function assumes that the formulas have been 
    -- 'simplify'ed. The intention is to make this explicit via a @newtype@ at
    -- some point.
    pattern :: (Monad m)
            => term -- ^ The 'pattern' formula to be filled in.
            -> term -- ^ The formula to fill in the pattern.
            -> m (Substitution ext)
    pattern = patternContinue M.empty


    -- | Same as 'pattern', but starts patterning from a given starting state.
    patternContinue :: (Monad m)
                    => Substitution ext
                    -> term
                    -> term
                    -> m (Substitution ext)
    patternContinue m a b = maybe (fail "pattern failed") return $ execStateT (patternM a b) m


    -- | Auxiliary monad, accompanying the 'pattern' function: assign variables
    -- from the first formula to subformulas of the second formula, step by 
    -- step, so as to obtain a substitution that would make the former equal to 
    -- the latter. In order to be able to short-circuit as early as possible, 
    -- this is done in a combination of the Maybe and State monads.
    patternM :: ()
             => term 
             -> term 
             -> StateFail (Substitution ext) ()



instance (Eq ext, Substitutable ext a) => Substitutable ext (Marked a) where
    patternM (Marked m1 x1) (Marked m2 x2) = do
        when (not $ all (`elem` m2) m1 && all (`elem` m1) m2) shortcircuit
        patternM x1 x2

    substitute subst (Marked marks x) = Marked marks <$> substitute subst x



instance (Eq ext, Substitutable ext ext) => Substitutable ext (Term ext) where
    patternM (Formula f1) (Formula f2) = patternM f1 f2
    patternM (Extension e1) (Extension e2) = patternM e1 e2
    patternM (MarkedFormula f1) (MarkedFormula f2) = patternM f1 f2
    patternM _ _ = shortcircuit

    substitute subst (Formula f) = Formula <$> substitute subst f
    substitute subst (Extension e) = Extension <$> substitute subst e
    substitute subst (MarkedFormula f) = MarkedFormula <$> substitute subst f


instance (Eq ext, Substitutable ext ext) => Substitutable ext (Formula ext) where

    patternM (Variable var) term = 
        var `binds` Formula term
    patternM (Implication a1 a2) (Implication b1 b2) = do
        a1 `patternM` b1
        a2 `patternM` b2
    patternM (Extend e a) (Extend e' a') = do
        e  `patternM` e'
        a  `patternM` a'
    patternM (Constant a) (Constant b) = 
        when (a /= b) shortcircuit
    patternM _ _ = shortcircuit

    substitute subst term = case term of
        Variable var -> case M.lookup var subst of
            Just (Formula t') -> return t'
            Just _ -> fail $ "gap requires different type at '" ++ var ++ "'"
            _ -> fail $ "variable '" ++ var ++ "' undefined"
        Constant a -> return $ Constant a
        Implication a b -> Implication <$> substitute subst a <*> substitute subst b
        Extend e a -> Extend <$> substitute subst e <*> substitute subst a



instance Substitutable Justification Justification where

    patternM (ProofVariable var) term = 
        var `binds` Extension term
    patternM (ProofChecker a) (ProofChecker b) = 
        a  `patternM` b
    patternM (Application a1 a2) (Application b1 b2) = do
        a1 `patternM` b1
        a2 `patternM` b2
    patternM (Sum a1 a2) (Sum b1 b2) = do
        a1 `patternM` b1
        a2 `patternM` b2
    patternM (ProofConstant a) (ProofConstant b) = 
        when (a /= b) shortcircuit
    patternM _ _ = shortcircuit

    substitute subst term = case term of
        ProofVariable var -> case M.lookup var subst of
            Just (Extension t') -> return t'
            Just _ -> fail $ "gap requires different type at '" ++ var ++ "'"
            _ -> fail $ "variable '" ++ var ++ "' undefined"
        ProofConstant c -> return (ProofConstant c)
        ProofChecker s -> ProofChecker <$> substitute subst s
        Application s t -> Application <$> substitute subst s <*> substitute subst t
        Sum s t -> Sum <$> substitute subst s <*> substitute subst t




-- | Auxiliary: Lifts failure from an underlying Maybe monad from within a
-- monad transformer.
shortcircuit :: MonadTrans t => t Maybe a
shortcircuit = lift Nothing


-- | Auxiliary: Try to add or verify a binding to a substitution monad. Fail
-- if the binding conflicts with an existing one in the substitution state.
binds :: (Eq ext)
      => String
      -> Term ext 
      -> StateFail (Substitution ext) ()
binds k v = maybe (shortcircuit) (void . put) . insert k v =<< get


-- | Insert value @v@ at key @k@. If there is already a value at @k@, this will
-- fail, unless the existing value at @k@ is identical to the new one.
insert :: (Ord k, Eq v, Monad m) => k -> v -> M.Map k v -> m (M.Map k v)
insert k v m = sequence $ M.insertWith identical k (return v) (fmap return m)


-- | Combine two substitutions, but fail if they are conflicting. 
--
-- Note: The union is more efficient if the biggest set is the first argument.
merge :: (Ord k, Eq v, Monad m) => M.Map k v -> M.Map k v -> m (M.Map k v)
merge a b = sequence $ M.unionWith identical (return <$> a) (return <$> b)


-- | Fail if the results of two actions are not the same. In the context of the
-- Maybe monad: only return a value if the first and second argument values
-- are present and equal to eachother.
identical :: (Eq a, Monad m) => m a -> m a -> m a
identical xm ym = do
    x <- xm
    y <- ym
    if x == y then return x 
              else fail "Conflicting assignment."


-- | Execute our state monad given a start state, and collect both its final 
-- state and its result.
execute :: state -> StateFail state result -> Maybe (state, result)
execute startstate monad = flip evalStateT startstate $ do
    result <- monad
    state <- get
    return (state, result)
