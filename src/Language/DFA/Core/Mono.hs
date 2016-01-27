{-# LANGUAGE TemplateHaskell, RecordWildCards,
             FlexibleContexts, LambdaCase #-}

-- The Monotone Framework

module Language.DFA.Core.Mono where

import qualified Data.Set  as S
import qualified Data.Map  as M
import Control.Lens
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

data CtxOp a l = PushCtx (M.Map a l) [Edge a]
               | PopCtx (l -> l -> l)

data Analysis ast blk l a = Analysis {
  _lattice      :: ast a -> Lattice l
, _extermals    :: ast a -> S.Set a
, _initSol      :: ast a -> M.Map a l
, _flow         :: ast a -> S.Set (Edge a)
, _interflow    :: ast a -> S.Set (a, a, a, a)
, _transfer     :: ast a -> (blk, a) -> l -> l
, _labels       :: ast a -> S.Set a
, _direction    :: Direction
, _blocks       :: ast a -> M.Map a blk
, _ctxOp        :: ast a -> a -> a -> l -> CtxOp a l
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
                        block = unsafeLookup a $ _blocks ast
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
        bs   = _blocks ast

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

---- Contextual Analysis ----

type CallString a = [a]

data SolutionInterp a l = SolutionInterp {
  _curCtx    :: CallString a
, _suspended :: M.Map (CallString a) (M.Map a l)
}

instance (Show a, Show l) => Show (SolutionInterp a l) where
    show s@SolutionInterp{..} =
        "Current Context: " ++ show _curCtx ++ "\n" ++
        "Suspended:\n" ++ unlines (map (\(cs, prop) -> show cs ++ "\t" ++ show (M.toList prop)) (M.toList _suspended))

analyzeInterp :: (Ord a, Eq a, Show l, Eq (Solution a l), Show a, Show (Solution a l)) =>
                 DebugOption -> Analysis ast blk l a -> ast a -> SolutionInterp a l
analyzeInterp opt analysis@Analysis{..} ast = iter initSol flow
    where
        initSol = SolutionInterp [] (M.singleton [] (_initSol ast))

        interflow = S.toList $ _interflow ast
        flow  = S.toList (_flow ast) ++ concatMap (\(lc, ln, lx, lr) -> [ Interp (lc, ln), Interp (lx, lr)] ) interflow
        bs    = _blocks ast

        iter sol []     = sol
        iter sol (w:ws) = case w of
            Intrap (l, l') -> withIntrap sol $ \cont sol ->
                let old      = unsafeLookup' $(__LOCATION__) l' sol
                    improved = _transfer ast (unsafeLookup' $(__LOCATION__) l bs, l) (unsafeLookup' $(__LOCATION__) l sol)
                    met      = (_meet lattice) old improved
                    wplus    = filter (\case
                                            Intrap (l1, _) -> l1 == l'
                                            Interp (l1, _) -> l1 == l') flow
                    ifLess   = (_lessThen lattice) improved old
                in  if not (ifLess)
                        then traceLog sol ws $ cont (M.insert l' met sol) (ws ++ wplus)
                        else traceLog sol ws $ cont sol ws
            Interp (l, l') ->
                let prop = curPropWithLabel sol l
                in case _ctxOp ast l l' prop of
                    PushCtx ctxInitSol wplus ->
                        let newCtx  = l : (_curCtx sol)
                            newSusp = M.insert newCtx ctxInitSol (_suspended sol)
                        in  iter (SolutionInterp newCtx newSusp) (wplus ++ ws)
                    PopCtx f ->
                        let curCtx   = _curCtx sol
                            curProp  = unsafeLookup' $(__LOCATION__) curCtx (_suspended sol)
                            oldCtx   = tail curCtx
                            oldProp  = unsafeLookup' $(__LOCATION__) oldCtx (_suspended sol)
                            oldProp' = f (unsafeLookup' $(__LOCATION__) l curProp) (unsafeLookup' $(__LOCATION__) l' oldProp)
                            susp'    = M.insert oldCtx (M.insert l' oldProp' oldProp)
                                                       (M.delete curCtx $ _suspended sol)
                            wplus    = filter (\case
                                            Intrap (l1, _) -> l1 == l'
                                            Interp (l1, _) -> l1 == l') flow
                        in  iter (SolutionInterp oldCtx susp') (ws ++ wplus)

        lattice = _lattice ast

        traceLog arr ws x =
            case opt of
                NoTrace   -> x
                ShowTrace -> trace ("------ Iteration ------\n" ++ show (M.toList arr) ++
                                    "\nWorklist: " ++ show ws ++ "\n") x

        withIntrap s cb =
            let curCtx  = _curCtx s
                curProp = unsafeLookup curCtx (_suspended s)
            in  cb (\curProp' ws' ->
                        iter (SolutionInterp curCtx $ M.insert curCtx curProp' (_suspended s)) ws'
                        ) curProp

        curPropWithLabel s l =
            let prop = unsafeLookup (_curCtx s) (_suspended s)
            in  unsafeLookup l prop

--- Misc

entry' :: Functor f => Direction -> (M.Map a l -> f (M.Map a l)) -> (Solution a l -> f (Solution a l))
entry' Forward  = entry
entry' Backward = exit

exit' :: Functor f => Direction -> (M.Map a l -> f (M.Map a l)) -> (Solution a l -> f (Solution a l))
exit' Forward  = exit
exit' Backward = entry

