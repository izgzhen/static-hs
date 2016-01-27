{-# LANGUAGE LambdaCase #-}

-- Detection of Sign Analysis
-- Featuring interprocedural analysis

module Language.DFA.Packages.DSA where

import Language.DFA.Core.Mono
import Language.DFA.AST
import Language.DFA.Core.Label
import Language.DFA.Common

import Data.Set hiding (filter, map, foldr)
import qualified Data.Set as S
import qualified Data.Map as M


data Sign = Positive | Zero     | Negative
          | Bottom   | Top
          deriving (Show, Eq, Ord)

sLE :: Sign -> Sign -> Bool
sLE Bottom   _        = True
sLE _        Top      = True
sLE Positive Positive = True
sLE Zero     Zero     = True
sLE Negative Negative = True
sLE _        _        = False

sMeet :: Sign -> Sign -> Sign
sMeet _      Top    = Top
sMeet Top    _      = Top
sMeet x      Bottom = x
sMeet Bottom x      = x
sMeet _      _      = Top

type DSProperty = M.Map Name Sign

dsLattice :: Label a => Program a -> Lattice DSProperty
dsLattice (Program _ topLevel) = Lattice {
  _lessThen = dsLE
, _meet     = dsMeet
, _bottom   = M.fromList $ zip (fv topLevel) (repeat Bottom)
}

dsLE :: DSProperty -> DSProperty -> Bool
dsLE ds1 ds2 = all id $ map (\(x, s1) -> s1 `sLE` unsafeLookup x ds2) $ M.toList ds1

dsMeet :: DSProperty -> DSProperty -> DSProperty
dsMeet ds1 ds2 = M.mapWithKey (\x s1 -> s1 `sMeet` unsafeLookup x ds2) ds1

dsAnalysis :: (Show a, Label a) => Analysis Program Block DSProperty a
dsAnalysis = Analysis {
  _lattice      = dsLattice
, _extermals    = S.singleton . initLabel
, _initSol      = dsInitSol
, _flow         = flow
, _transfer     = dsTranfer
, _labels       = labels
, _direction    = Forward
, _blocks       = blocks
, _ctxOp        = dsCtxOp
}

dsInitSol :: Label a => Program a -> M.Map a DSProperty
dsInitSol (Program _ stmt) =
    M.fromList $ zip (toList $ labels stmt) $
                     repeat (M.fromList $ zip (fv stmt)
                                              (repeat Bottom))

dsTranfer :: Label a => Program a -> (Block, a) -> DSProperty -> DSProperty
dsTranfer _ (block, l) entered = case block of
        BAssign x a -> M.insert x (acp a entered) entered
        BSkip       -> entered
        BBExp bexp  -> entered
    where
        acp :: AExp -> DSProperty -> Sign
        acp (AVar x) p = unsafeLookup x p
        acp (ANum i) _ = if i > 0 then Positive else if i < 0 then Negative else Zero
        acp (AInfix a1 aop a2) p =
            applyAOpDS (acp a1 p, aop, acp a2 p)

applyAOpDS :: (Sign, AOp, Sign) -> Sign
applyAOpDS = \case
    (Positive, Plus,     Positive)  -> Positive
    (Zero,     Plus,     Zero)      -> Zero
    (Negative, Plus,     Negative)  -> Negative
    (x,        Plus,     Zero)      -> x
    (Zero,     Plus,     x)         -> x
    (Positive, Minus,    Negative)  -> Positive
    (Zero,     Minus,    Zero)      -> Zero
    (Negative, Minus,    Positive)  -> Negative
    (x,        Minus,    Zero)      -> x
    (Zero,     Minus,    Negative)  -> Positive
    (Zero,     Minus,    Positive)  -> Negative
    (Positive, Multiply, Positive)  -> Positive
    (Negative, Multiply, Negative)  -> Positive
    (Negative, Multiply, Positive)  -> Negative
    (Positive, Multiply, Negative)  -> Negative
    (Zero,     Multiply, _)         -> Zero
    (_,        Multiply, Zero)      -> Zero
    _                               -> Top

-- Interp flow (l, l')
dsCtxOp :: (Show a, Label a) => Program a -> a -> a -> DSProperty -> CtxOp a DSProperty
dsCtxOp p@(Program procs _) l l' prop =
    case (unsafeLookup l bs, unsafeLookup l' bs) of
        (BCall f ins outs, BIs) ->
            let proc = head $ filter (\(Proc f' _ _ _ _ _) -> f' == f) procs
                Proc _ ins' outs' _ stmt _ = proc
                prop' = M.fromList $ zip ins' $ map (flip unsafeLookup prop) ins
                dict  = zip (S.toList $ labels proc) $
                            repeat (M.fromList $ zip (fv stmt)
                                                     (repeat Bottom))
            in  PushCtx $ M.insert l' prop' (M.fromList dict)
        (BEnd, BCall f ins outs) -> PopCtx $ \retsiteProp callsiteProp ->
                    let proc = head $ filter (\(Proc f' _ _ _ _ _) -> f' == f) procs
                        Proc _ ins' outs' _ stmt _ = proc
                        propRet = zip outs $ map (flip unsafeLookup prop) outs'
                        callsiteProp' = foldr meet callsiteProp propRet
                        meet (x, px) p = M.insert x (px `sMeet` unsafeLookup x p) p
                    in  callsiteProp'
        _ -> error $ "Illegal interprocedural flow labels: " ++ show l ++ ", " ++ show l'
    where
        bs = blocks p


dsa :: (Label a, Show a) =>
       DebugOption -> Program a -> SolutionInterp a DSProperty
dsa opt = analyzeInterp opt dsAnalysis

