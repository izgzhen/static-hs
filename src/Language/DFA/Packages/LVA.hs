{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- Live Variables Analysis

module Language.DFA.Packages.LVA where

import Language.DFA.AST
import Language.DFA.Core.Label
import Language.DFA.Core.Mono
import Language.DFA.Common

import Data.Set hiding (filter)
import qualified Data.Map as M
import Control.Lens

type LVProperty = Set Name

type LVSolution a = Solution a LVProperty

lvLattice :: Lattice LVProperty
lvLattice = Lattice {
  _lessThen = isSubsetOf
, _meet     = union
, _bottom   = empty
}

lvAnalysis :: Label a => Analysis Stmt Block LVProperty a
lvAnalysis = Analysis {
  _lattice   = \_ -> lvLattice
, _extermals = finalLabels
, _initSol   = lvInitSol
, _flow      = reverseFlow
, _interflow = undefined
, _transfer  = lvTransfer
, _labels    = labels
, _direction = Backward
, _toBlocks  = toBlocks
, _getContextOp = undefined
}

lvInitSol :: Label a => Stmt a -> M.Map a LVProperty
lvInitSol stmt = M.fromList $ zip (toList $ labels stmt) $ repeat empty

lvTransfer :: Label a => Stmt a -> (Block, a) -> LVProperty -> LVProperty
lvTransfer _ (block, _) exited = exited \\ kill block `union` gen block
    where
        -- kill and gen functions
        kill :: Block -> LVProperty
        kill (BAssign x a) = singleton x
        kill _             = empty

        gen :: Block -> LVProperty
        gen (BAssign x a) = fv a
        gen BSkip         = empty
        gen (BBExp bexp)  = fv bexp

lva :: (Label a, Eq (LVSolution a), Show (LVSolution a)) =>
       DebugOption -> Stmt a -> Solution a LVProperty
lva opt = analyze opt lvAnalysis

