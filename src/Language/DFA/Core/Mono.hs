{-# LANGUAGE TemplateHaskell, RecordWildCards,
             FlexibleContexts, LambdaCase #-}

-- The Monotone Framework

module Language.DFA.Core.Mono where

import qualified Data.Set  as S
import qualified Data.Map  as M
import Control.Lens hiding (Context)
import Debug.Trace
import Debug.Trace.LocationTH

import Language.DFA.Core.Label
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
, _flow         :: ast a -> S.Set (Edge a)
, _interflow    :: ast a -> S.Set (a, a, a, a)
, _transfer     :: ast a -> (blk, a) -> l -> l
, _labels       :: ast a -> S.Set a
, _direction    :: Direction
, _toBlocks     :: ast a -> M.Map a blk
, _getContextOp :: a -> a -> ast a -> ContextOp a l blk
}

---- Coarse Chaotic Iteration ----

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
                        block = unsafeLookup a $ _toBlocks ast
                        prop  = unsafeLookup a (sol' ^. entry' _direction)
                        prop' = _transfer ast (block, a) prop
                    in  (exit' _direction) %~ (M.insert a prop') $ sol'

        -- converge :: (Ord a, Eq a) => Analysis ast blk l a -> ast a -> a -> Solution a l -> Solution a l
        converge analysis@Analysis{..} ast l sol
            | l `S.member` _extermals ast = sol
            | otherwise                   = entry' _direction %~ M.insert l s $ sol
                where s = foldr (_meet lattice) (_bottom lattice)
                                [ unsafeLookup l' (sol ^. exit' _direction)
                                | Intrap (l', l'') <- S.toList $ _flow ast
                                , l == l'' ]

                      lattice = _lattice ast

---- Better granularity with MFP and worklist ----

analyze' :: (Show l, Show a, Ord a, Eq a, Eq (Solution a l)) =>
           DebugOption -> Analysis ast blk l a -> ast a -> Solution a l
analyze' opt analysis@Analysis{..} ast =
    let arr' = iter (_initSol ast) flow
    in  Solution arr' (M.mapWithKey (\i prop -> _transfer ast (unsafeLookup i bs, i) prop) arr')
    where
        flow = S.toList $ _flow ast
        bs   = _toBlocks ast

        iter arr []     = arr
        iter arr (w:ws) =
            let Intrap (l, l') = w
                old      = unsafeLookup l' arr
                improved = _transfer ast (unsafeLookup l bs, l) (unsafeLookup l arr)
                met      = (_meet lattice) old improved
                wplus    = filter (\(Intrap (l1, _)) -> l1 == l') flow
                ifLess   = (_lessThen lattice) improved old
            in  if not (ifLess)
                    then traceLog arr ws $ iter (M.insert l' met arr) (ws ++ wplus)
                    else traceLog arr ws $ iter arr ws

        lattice = _lattice ast

        traceLog arr ws x =
            case opt of
                NoTrace   -> x
                ShowTrace -> trace ("------ Iteration ------\n" ++ show arr ++ "\n" ++ show ws) x

---- Interprocedural Analysis ----

type InterpState a l blk = [Context a l blk]

data Context a l blk = Context {
  _curflow  :: S.Set (Edge a)   -- flowgraph of current context
, _worklist :: [Edge a]
, _property :: M.Map a l
, _blocks   :: M.Map a blk
}

data ContextOp a l blk = EnterCtx (l -> l) (S.Set (Edge a)) (M.Map a blk) (M.Map a l)
                       | ExitCtx l

analyzeInterp :: Ord a => DebugOption -> Analysis ast blk l a -> ast a -> InterpState a l blk
analyzeInterp opt analysis@Analysis{..} prog =
    let initialState = [ Context (_flow prog) (S.toList $ _flow prog)
                                 (_initSol prog) (_toBlocks prog) ] -- top-level context
    in  iter initialState
    where
        iter (curCtx : prevCtxs) =
            case (_worklist curCtx) of
                Interp (l, l') : ws ->
                    case _getContextOp l l' prog of
                        EnterCtx intro flow blocks initSol ->
                            let callSiteProp = unsafeLookup l (_property curCtx)
                                entryProp = M.insert l' (intro callSiteProp) initSol
                                newCtx = Context flow (S.toList flow)
                                                 entryProp blocks
                            in  iter (newCtx : curCtx { _worklist = ws } : prevCtxs)
                        ExitCtx refine ->
                            let callerCtx : prevCtxs' = prevCtxs
                                newProp = M.update (Just . meet refine) l' (_property callerCtx)
                            in  iter (callerCtx { _property = newProp } : prevCtxs')
                Intrap (l, l') : ws ->
                    let old      = unsafeLookup l' (_property curCtx)
                        improved = transfer (unsafeLookup l (_blocks curCtx), l)
                                            (unsafeLookup l $ _property curCtx)
                        met      = old `meet` improved
                        wplus    = filter (\(Intrap (l1, _)) -> l1 == l') $ S.toList (_curflow curCtx)
                    in  if not (improved `lessThen` old)
                            then iter $ curCtx { _property = M.insert l' met (_property curCtx)
                                               , _worklist = ws ++ wplus } : prevCtxs
                            else iter $ curCtx { _worklist = ws } : prevCtxs

        lattice  = _lattice prog
        meet     = _meet lattice
        lessThen = _lessThen lattice
        transfer = _transfer prog


--- Misc

entry' :: Functor f => Direction -> (M.Map a l -> f (M.Map a l)) -> (Solution a l -> f (Solution a l))
entry' Forward  = entry
entry' Backward = exit

exit' :: Functor f => Direction -> (M.Map a l -> f (M.Map a l)) -> (Solution a l -> f (Solution a l))
exit' Forward  = exit
exit' Backward = entry

