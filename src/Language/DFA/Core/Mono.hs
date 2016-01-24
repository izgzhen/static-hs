{-# LANGUAGE TemplateHaskell, RecordWildCards,
             FlexibleContexts #-}

-- The Monotone Framework

module Language.DFA.Core.Mono where

import qualified Data.Set  as S
import qualified Data.Map  as M
import Control.Lens
import Debug.Trace

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

data Analysis ast blk l a = Analysis {
  _lattice      :: ast a -> Lattice l
, _extermals    :: ast a -> S.Set a
, _initSol      :: ast a -> M.Map a l
, _flow         :: ast a -> S.Set (a, a)
, _transfer     :: ast a -> (blk, a) -> l -> l
, _labels       :: ast a -> S.Set a
, _direction    :: Direction
, _blocks       :: ast a -> M.Map a blk
}

converge :: (Ord a, Eq a) => Analysis ast blk l a -> ast a -> a -> Solution a l -> Solution a l
converge analysis@Analysis{..} ast l sol
    | l `S.member` _extermals ast = sol
    | otherwise                   = entry' _direction %~ M.insert l s $ sol
        where s = foldr (_meet lattice) (_bottom lattice)
                        [ unsafeLookup l' (sol ^. exit' _direction)
                        | (l', l'') <- S.toList $ _flow ast
                        , l == l'' ]

              lattice = _lattice ast

-- Coarse Chaotic Iteration
analyze :: (Ord a, Eq a, Eq (Solution a l), Show (Solution a l)) =>
           DebugOption -> Analysis ast blk l a -> ast a -> Solution a l
analyze opt analysis@Analysis{..} ast = chaotic opt initSol improveSol
    where
        initSol = Solution (_initSol ast) (_initSol ast)
        -- improveSol :: Solution a l -> Solution a l
        improveSol = foldr1 (.) $ map with $ S.toList (_labels ast)
            where
                -- with :: a -> (Solution a l -> Solution a l)
                with a sol =
                    let sol'  = converge analysis ast a sol
                        block = unsafeLookup a $ _blocks ast
                        prop  = unsafeLookup a (sol' ^. entry' _direction)
                        prop' = _transfer ast (block, a) prop
                    in  (exit' _direction) %~ (M.insert a prop') $ sol'

-- Better granularity with MFP and worklist
analyze' :: (Show l, Show a, Ord a, Eq a, Eq (Solution a l)) =>
           DebugOption -> Analysis ast blk l a -> ast a -> Solution a l
analyze' opt analysis@Analysis{..} ast =
    let arr' = iter (_initSol ast) flow
    in  Solution arr' (M.mapWithKey (\i prop -> _transfer ast (unsafeLookup i bs, i) prop) arr')
    where
        flow = S.toList $ _flow ast
        bs   = _blocks ast

        iter arr []     = arr
        iter arr (w:ws) =
            let (l, l')  = w
                old      = unsafeLookup l' arr
                improved = _transfer ast (unsafeLookup l bs, l) (unsafeLookup l arr)
                met      = (_meet lattice) old improved
                wplus    = filter (\(l1, _) -> l1 == l') flow
                ifLess   = (_lessThen lattice) improved old
            in  if not (ifLess)
                    then traceLog arr ws $ iter (M.insert l' met arr) (ws ++ wplus)
                    else traceLog arr ws $ iter arr ws

        lattice = _lattice ast

        traceLog arr ws x =
            case opt of
                NoTrace   -> x
                ShowTrace -> trace ("------ Iteration ------\n" ++ show arr ++ "\n" ++ show ws) x

entry' :: Functor f => Direction -> (M.Map a l -> f (M.Map a l)) -> (Solution a l -> f (Solution a l))
entry' Forward  = entry
entry' Backward = exit

exit' :: Functor f => Direction -> (M.Map a l -> f (M.Map a l)) -> (Solution a l -> f (Solution a l))
exit' Forward  = exit
exit' Backward = entry

