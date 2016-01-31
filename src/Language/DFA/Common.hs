{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             DataKinds, PolyKinds #-}

-- Common Imports

module Language.DFA.Common where

import Data.Set (Set, singleton)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Name = String

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

unsafeLookup' :: Ord k => String -> k -> M.Map k v -> v
unsafeLookup' hint k m =
    case M.lookup k m of
        Just x  -> x
        Nothing -> error $ "unsafeLookup failed: " ++ hint

data DebugOption = ShowTrace | NoTrace

-- Fetch `e` contained in `m` from recursive data structure `a`
class Recursive a e t where
    recursive :: Container m e => t -> a -> m e

-- Enhance Monoid with `single`, similar to `pure`
class Monoid (m e) => Container m e where
    single :: e -> m e

-- Collect `m` from a recursive data structure `a` recursively
class Collect a where
    collect :: Monoid m => (a -> m) -> a -> m

instance Ord a => Container Set a where
    single = singleton

instance Container [] a where
    single x = [x]
