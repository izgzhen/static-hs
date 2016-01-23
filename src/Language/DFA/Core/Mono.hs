{-# LANGUAGE TemplateHaskell, RecordWildCards,
             FlexibleContexts #-}

-- The Monotone Framework

module Language.DFA.Core.Mono where

import qualified Data.Set  as S
import qualified Data.Map  as M
import Control.Lens

import Language.DFA.Core.Iteration
import Language.DFA.Common

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

data Direction = Forward | Backward

data Analysis ast l a = Analysis {
  _lattice      :: ast a -> Lattice l
, _extermals    :: ast a -> S.Set a
, _initSol      :: ast a -> Solution a l
, _flow         :: ast a -> S.Set (a, a)
, _transfer     :: ast a -> a -> Solution a l -> Solution a l
, _labels       :: ast a -> S.Set a
, _direction    :: Direction
}

converge :: (Ord a, Eq a) => Analysis ast l a -> ast a -> a -> Solution a l -> Solution a l
converge analysis@Analysis{..} ast l sol
    | l `S.member` _extermals ast = sol
    | otherwise                   = entry' _direction %~ M.insert l s $ sol
        where s = foldr (_meet lattice) (_bottom lattice)
                        [ unsafeLookup l' (sol ^. exit' _direction)
                        | (l', l'') <- S.toList $ _flow ast
                        , l == l'' ]

              entry' Forward  = entry
              entry' Backward = exit

              exit'  Forward  = exit
              exit'  Backward = entry

              lattice = _lattice ast

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

