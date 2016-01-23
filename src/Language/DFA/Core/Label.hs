{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FlexibleInstances #-}

module Language.DFA.Core.Label where

-- Abstract Label Type and Instance for Stmt AST

import Data.Set hiding (foldr)
import qualified Data.Set as S

class (Ord a, Eq a) => Label a where

class Label a => Labelled ast a where
    initLabel   :: ast a -> a
    finalLabels :: ast a -> Set a
    labels      :: ast a -> Set a
    flow        :: ast a -> Set (a, a)
    reverseFlow :: ast a -> Set (a, a)

    reverseFlow = S.map (\(a, b) -> (b, a)) . flow

