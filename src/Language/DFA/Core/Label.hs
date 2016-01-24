{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FlexibleInstances #-}

-- Abstract Label Type

module Language.DFA.Core.Label where

import Data.Set hiding (foldr)
import qualified Data.Set as S

class (Ord a, Eq a) => Label a where

class Label a => Labelled ast a where
    initLabel   :: ast a -> a
    finalLabels :: ast a -> Set a
    flow        :: ast a -> Set (a, a)
    reverseFlow :: ast a -> Set (a, a)

    reverseFlow = S.map (\(a, b) -> (b, a)) . flow

class Label a => InterLabelled ast a where
    interflow :: ast a -> Set (a, a, a, a)

