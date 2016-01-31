{-# LANGUAGE TemplateHaskell #-}
-- Some common utilities for interprocedual analysis

module Language.DFA.Packages.Interp where

import Language.DFA.AST
import Language.DFA.Core.Label
import Language.DFA.Core.Mono
import Language.DFA.Common

import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace.LocationTH

interpFlow :: Label a => Program a -> S.Set (Edge a)
interpFlow prog@(Program _ stmt) = flow stmt `S.union`
     S.fromList [ Interp (lc, ln)
                | (lc, ln, _, _) <- S.toList $ interflow prog
                , lc `S.member` labels stmt ]

interpFlowProc :: InterLabelled ast a => ast a -> Proc a -> S.Set (Edge a)
interpFlowProc prog proc@(Proc _ _ _ _ _ end) = flow proc `S.union`
    S.fromList [ Interp (lx, lr)
               | (_, _, lx, lr) <- S.toList $ interflow prog
               , lx `S.member` labels proc ]


-- Interp flow (l, l')
getContextOp :: (Label a, Show a) =>
                (xl -> xl -> xl) -> xl -> a -> a -> Program a -> ContextOp a (M.Map Name xl) Block
getContextOp xMeet bottom l l' p@(Program procs _) =
    case (unsafeLookup' $__LOCATION__ l bs, unsafeLookup' $__LOCATION__ l' bs) of
        (BCall f ins outs, BIs) ->
            let proc = head $ filter (\(Proc f' _ _ _ _ _) -> f' == f) procs
                Proc _ ins' outs' _ stmt _ = proc
                initSol prop =
                    let entry = M.fromList $ zip ins' (map (flip (unsafeLookup' $__LOCATION__) prop) ins)
                        emptyDict = zip (S.toList $ labels proc) $
                                        repeat (M.fromList $ zip (fv stmt)
                                                             (repeat bottom))
                    in  M.update (Just . M.union entry) l' (M.fromList emptyDict)
                botRet callSiteProp = M.mapWithKey (\x l ->
                                        if x `elem` outs
                                            then bottom
                                            else l `xMeet` unsafeLookup' $__LOCATION__ x callSiteProp)
            in  EnterCtx (interpFlowProc p proc) (toBlocks proc) initSol botRet

        (BEnd, BCall f ins outs) -> ExitCtx $ \calleeProp callerProp ->
            let proc = head $ filter (\(Proc f' _ _ _ _ _) -> f' == f) procs
                Proc _ ins' outs' _ stmt _ = proc
                updates = zip outs $ map (flip (unsafeLookup' $__LOCATION__) calleeProp) outs'
                callerProp' = foldr meet callerProp updates
                meet (x, px) p = M.insert x (px `xMeet` unsafeLookup' $__LOCATION__ x p) p
            in  callerProp'
        _ -> error $ "Illegal interprocedural flow labels: " ++ show l ++ ", " ++ show l'
    where
        bs = toBlocks p
