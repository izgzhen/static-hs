{-# LANGUAGE TemplateHaskell, RecordWildCards,
             FlexibleContexts #-}

-- The Monotone Framework

module Mono where

import qualified Data.Set  as S
import qualified Data.Map  as M
import Control.Lens
import Iteration
import Common

{-
    Property space: L
        + Complete lattice
        + Satisfies Ascending Chain Condition
        + Having a least upper bound operator `U`

    Transfer functions: F
        + L -> L
        + Contains identity function
        + Closed under function composition

    Distributivity:
        + f (l1 U l2) = f(l1) U f(l2)

    Flow:
        + flow(S)
        + reversed flow(S)

    Extermal labels:
        + init(S)
        + final(S)

    Extremal value:
        + Some l in L for extermal labels

    Mapping f:
        + Label -> F
-}


-- summarizeSingleStep src l sol =
--     foldr1 union [ reflect l' | (l', l) <- flow src ] `union` extermal
--         where
--             extermal = if l `member` extermalLabels src
--                             then extermalProperty
--                             else lBot

-- reflectSingleStep src l sol = updateSol l (transfer l src sol) sol

data Solution a l = Solution {
  _entry :: M.Map a l
, _exit  :: M.Map a l
} deriving (Eq)

instance (Show a, Show l) => Show (Solution a l) where
    show sol@Solution{..} =
        unlines $ "Entry" : f _entry ++ ["Exit:"] ++ f _exit
            where
                f = M.elems . M.mapWithKey
                        (\i ps -> show i ++ " : " ++ show ps)

makeLenses ''Solution

data Lattice l = Lattice {
  -- ⊑
  _lessThen :: l -> l -> Bool
  -- ⊔
, _meet :: l -> l -> l
  -- ⊥
, _bottom :: l
}

data Analysis ast l a = Analysis {
  _lattice      :: Lattice l
, _extermals    :: ast a -> S.Set a
, _initSol      :: ast a -> Solution a l
, _flow         :: ast a -> S.Set (a, a)
, _transfer     :: ast a -> a -> Solution a l -> Solution a l
, _labels       :: ast a -> S.Set a
}


converge :: (Ord a, Eq a) => Analysis ast l a -> ast a -> a -> Solution a l -> Solution a l
converge analysis@Analysis{..} ast l sol
    | l `S.member` _extermals ast = sol
    | otherwise                 = entry %~ (M.insert l s) $ sol
        where s = foldr (_meet _lattice) (_bottom _lattice)
                        [ unsafeLookup l' (_exit sol) | (l', l'') <- S.toList $ _flow ast
                                                      , l == l'' ]

analyze :: (Ord a, Eq a, Eq (Solution a l), Show (Solution a l)) =>
           DebugOption -> Analysis ast l a -> ast a -> Solution a l
analyze opt analysis@Analysis{..} ast = chaotic opt (_initSol ast) improveSol
    where
        -- improveSol :: Solution a l -> Solution a l
        improveSol = foldr1 (.) $ map with $ S.toList (_labels ast)
            where
                -- with :: a -> (Solution a l -> Solution a l)
                with a = let f = converge analysis ast  a
                             g = _transfer ast a
                         in  g . f

