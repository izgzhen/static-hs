{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             DataKinds, PolyKinds #-}

-- Common Imports

module Language.DFA.Common where

import Data.Set (Set, singleton)
import qualified Data.Map as M
import Data.Maybe (fromJust)

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

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

{-
    Design note:

    Why not write `Recursive` and `Collect` together?

    Because of the `e`. The `e` is much alike used for type checking
    purpose. For example, the FV function which collect free variables
    recursively. We will instantiate a

        recursive :: Stmt a -> m Name
    
    and rename it to `fv`.
    
    Apparently, this is not a good idea. A better one would be declare
    some types:

        data Rec = FV | ...

    Here

        > :k Fv
        Fv :: *

    Then, for any recursive, we will attach another parameter to Recursive:

        class Recursive Rec a e where
    
    And for `fv`

        instance Recursive FV (Stmt a) Name where
    
-}