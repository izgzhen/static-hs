{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- Very Busy Expressions Analysis

module Language.DFA.Packages.VBEA where

import Language.DFA.Core.Mono
import Language.DFA.AST
import Language.DFA.Core.Label
import Language.DFA.Common

import Data.Set hiding (filter)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens

type VBEProperty = Set AExp

type VBESolution a = Solution a VBEProperty

vbeLattice :: Label a => Stmt a -> Lattice VBEProperty
vbeLattice stmt = Lattice {
  _lessThen = flip isSubsetOf -- isSupersetOf
, _meet     = intersection
, _bottom   = S.filter nonTrivial $ subAExps stmt
}

vbeAnalysis :: Label a => Analysis Stmt Block VBEProperty a
vbeAnalysis = Analysis {
  _lattice      = vbeLattice
, _extermals    = finalLabels
, _initSol      = vbeInitSol
, _flow         = reverseFlow
, _interflow    = undefined
, _transfer     = vbeTransfer
, _labels       = labels
, _direction    = Backward
, _toBlocks     = toBlocks
, _getContextOp = undefined
}

vbeInitSol :: Label a => Stmt a -> M.Map a VBEProperty
vbeInitSol stmt = M.fromList $ zip (toList $ labels stmt) $ repeat empty

vbeTransfer :: Label a => Stmt a -> (Block, a) -> VBEProperty -> VBEProperty
vbeTransfer stmt (block, _) entered = entered \\ kill (subAExps stmt) block `union` gen block
    where
        -- kill and gen functions
        kill :: Set AExp -> Block -> VBEProperty
        kill exps (BAssign x a) =
            fromList [ a' | a' <- filter nonTrivial (toList exps)
                          , x `member` fv a' ]
        kill _ _ = empty

        gen :: Block -> VBEProperty
        gen (BAssign x a) = S.filter nonTrivial (subAExps a)
        gen BSkip         = empty
        gen (BBExp bexp)  = S.filter nonTrivial (subAExps bexp)


vbea :: (Label a, Eq (VBESolution a), Show (VBESolution a)) =>
        DebugOption -> Stmt a -> Solution a VBEProperty
vbea opt = analyze opt vbeAnalysis

