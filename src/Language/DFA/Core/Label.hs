{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FlexibleInstances #-}

-- Abstract Label Type

module Language.DFA.Core.Label where

import Data.Set hiding (foldr)
import qualified Data.Set as S

data Edge a = Intrap (a, a)
            | Interp (a, a)
            deriving (Show, Eq, Ord)

class (Ord a, Eq a) => Label a where

class Label a => Labelled ast a where
    initLabel   :: ast a -> a
    finalLabels :: ast a -> Set a
    flow        :: ast a -> Set (Edge a)
    reverseFlow :: ast a -> Set (Edge a)

    reverseFlow = S.map rev . flow
        where
            rev (Interp (a, b)) = Interp (b, a)
            rev (Intrap (a, b)) = Intrap (b, a)

class Label a => InterLabelled ast a where
    interflow :: ast a -> Set (a, a, a, a)

