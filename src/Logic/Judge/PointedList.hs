{-|
Module      : Logic.Judge.PointedList
Description : Re-export 'Data.List.PointedList' with convenience functions.
Copyright   : (c) 2017 ns@slak.ws
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

Re-export 'Data.List.PointedList' with convenience functions.
-}

{-# LANGUAGE PackageImports #-}
module Logic.Judge.PointedList 
    ( module L
    , asList
    , toList
    , current
    , insertAll
    , focus
    , modify
    , update
    ) where

import "pointedlist" Data.List.PointedList as L hiding (focus)
import "base" Data.Foldable (toList)

-- | Turn a 'PointedList' into a non-pointed list, assuming no particular
-- ordering.
asList :: L.PointedList a -> [a]
asList (L.PointedList prefix x postfix) = x : prefix ++ postfix


-- | Get the current focus of a 'PointedList'.
current :: L.PointedList a -> a
current = L._focus


-- | Add a number of entries to a possibly empty 'PointedList'.
insertAll :: Traversable t => t a -> Maybe (L.PointedList a) -> Maybe (L.PointedList a)
insertAll xs Nothing  = L.fromList . toList $ xs
insertAll xs (Just l) = return . foldr L.insertLeft l $ xs


-- | Create a list of variations of the provided 'PointedList', one for each
-- element to take focus. 
--
-- This function is a bit sloppy. It assumes no particular order. Note that using
-- 'toList' instead can cause longer (or shorter?) running times.
focus :: L.PointedList a -> [L.PointedList a]
focus = asList . L.positions


-- | Update the focused element of a 'PointedList' using a function that
-- returns 'Just' the new value, or 'Nothing' for deletion.
modify :: L.PointedList a -> (a -> Maybe a) -> Maybe (L.PointedList a)
modify xs f = maybe (L.delete xs) (Just . flip L.replace xs) . f $ current xs


-- | Update the focused element of a 'PointedList' with the value of the
-- 'Just', or delete the element if that value is 'Nothing'.
update :: L.PointedList a -> Maybe a -> Maybe (L.PointedList a)
update xs = modify xs . const
