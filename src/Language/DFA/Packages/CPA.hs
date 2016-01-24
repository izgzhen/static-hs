{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- Constant Propagation Analysis
-- This is a non-distributive framework -- What is the different?

module Language.DFA.Packages.CPA where

import Language.DFA.Core.Mono
import Language.DFA.AST
import Language.DFA.Core.Label
import Language.DFA.Common

import Data.Set hiding (filter, map)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens hiding (Const)

data ExtendedInt = TopInt | BotInt | Const Int deriving (Eq, Ord, Show)

type CPProperty = Maybe (M.Map Name ExtendedInt)


cLE :: ExtendedInt -> ExtendedInt -> Bool
cLE _          TopInt     = True
cLE (Const z1) (Const z2) = z1 == z2
cLE BotInt     _          = True
cLE _          _          = False

cMeet :: ExtendedInt -> ExtendedInt -> ExtendedInt
cMeet TopInt     TopInt     = TopInt
cMeet TopInt     x          = x
cMeet x          TopInt     = x
cMeet (Const z1) (Const z2) = if z1 == z2 then Const z1 else BotInt
cMeet BotInt     _          = BotInt
cMeet _          BotInt     = BotInt

{-
    FIXME:

    `cMeet` looks weird to me. Let's rephrase it:

    * `TopInt`  is "non-constant"
    * `Const i` is "constant integer i"
    * `BotInt`  is "not initialized on every execution" or
                   "the statement is unreachable"
    
    Image that `TopInt` is on the top, `BotInt` is on the bottom,
    and every `Const i` is in the middle.
    
    There is also a truth table in
    http://www.cs.berkeley.edu/~bodik/cs264/lectures/4-chaotic-notes.pdf.
    But different definition is used and it makes things every more confusing.

-}

cpLE :: CPProperty -> CPProperty -> Bool
cpLE Nothing   _         = True
cpLE (Just p1) (Just p2) =
    all id $ map (\(x, c1) -> c1 `cLE` unsafeLookup x p2) $ M.toList p1
cpLE _         _         = False

cpMeet :: CPProperty -> CPProperty -> CPProperty
cpMeet Nothing   p         = p
cpMeet p         Nothing   = p
cpMeet (Just p1) (Just p2) = 
    Just $ M.mapWithKey (\x c1 -> c1 `cMeet` unsafeLookup x p2) p1

type CPSolution a = Solution a CPProperty

cpLattice :: Label a => Stmt a -> Lattice CPProperty
cpLattice stmt = Lattice {
  _lessThen = cpLE
, _meet     = cpMeet
, _bottom   = Nothing
}

cpAnalysis :: Label a => Analysis Stmt Block CPProperty a
cpAnalysis = Analysis {
  _lattice      = cpLattice
, _extermals    = singleton . initLabel
, _initSol      = cpInitSol
, _flow         = flow
, _transfer     = cpTransfer
, _labels       = labels
, _direction    = Forward
, _blocks       = blocks
}

cpInitSol :: Label a => Stmt a -> M.Map a CPProperty
cpInitSol stmt = initial
    where
        names   = S.toList $ fv stmt
        initial = M.fromList $ zip (toList $ labels stmt) $
                                   repeat $ Just (M.fromList (zip names (repeat BotInt)))

cpTransfer :: Label a => Stmt a -> (Block, a) -> CPProperty -> CPProperty
cpTransfer stmt (block, _) entered = case block of
        BAssign x a -> entered >>= \e -> return $ M.insert x (acp a entered) e
        BSkip       -> entered
        BBExp bexp  -> entered
    where
        acp :: AExp -> CPProperty -> ExtendedInt
        acp (AVar x) Nothing   = BotInt
        acp (AVar x) (Just p)  = unsafeLookup x p
        acp (ANum i) Nothing   = BotInt
        acp (ANum i) (Just p)  = Const i
        acp (AInfix a1 aop a2) p =
            applyAOpExt aop (acp a1 p) (acp a2 p)

applyAOpExt :: AOp -> ExtendedInt -> ExtendedInt -> ExtendedInt
applyAOpExt aop (Const z1) (Const z2) = Const $ applyAOp aop z1 z2
applyAOpExt _   BotInt     _          = BotInt
applyAOpExt _   _          BotInt     = BotInt
applyAOpExt _   _          _          = TopInt

cpa :: (Label a, Eq (CPSolution a), Show (CPSolution a)) =>
       DebugOption -> Stmt a -> Solution a CPProperty
cpa opt = analyze opt cpAnalysis

