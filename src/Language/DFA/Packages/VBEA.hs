{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- Very Busy Expressions Analysis

module Language.DFA.Packages.VBEA where

import Language.DFA.Core.Mono
import Language.DFA.AST
import Language.DFA.AST.Recursive
import Language.DFA.AST.Label
import Language.DFA.AST.Block
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

vbeAnalysis :: Label a => Analysis Stmt VBEProperty a
vbeAnalysis = Analysis {
  _lattice      = vbeLattice
, _extermals    = finalLabels
, _initSol      = vbeInitSol
, _flow         = reverseFlow
, _transfer     = vbeTransfer
, _labels       = labels
, _direction    = Backward
}

vbeInitSol :: Label a => Stmt a -> VBESolution a
vbeInitSol stmt = Solution initial initial
    where
        initial = M.fromList $ zip (toList $ labels stmt) $ repeat empty

vbeTransfer :: Label a => Stmt a -> a -> VBESolution a -> VBESolution a
vbeTransfer stmt l sol =
    let exps        = subAExps stmt
        block       = head $ filter (\b -> labelOfBlock b == l) $ blocks stmt
        enteredExps = unsafeLookup l (_exit sol)
        s           = (enteredExps \\ kill exps block) `union` gen block
    in  entry %~ (M.insert l s) $ sol
    where
        -- kill and gen functions
        kill :: Set AExp -> Block a -> VBEProperty
        kill exps (BStmt (Assign l x a)) =
            fromList [ a' | a' <- filter nonTrivial (toList exps)
                          , x `member` fv a' ]
        kill _ _ = empty

        gen :: Block a -> VBEProperty
        gen (BStmt (Assign _ x a)) = S.filter nonTrivial (subAExps a)
        gen (BStmt (Skip _))       = empty
        gen (BBExp (bexp, _))      = S.filter nonTrivial (subAExps bexp)


vbea :: (Label a, Eq (VBESolution a), Show (VBESolution a)) =>
        DebugOption -> Stmt a -> Solution a VBEProperty
vbea opt = analyze opt vbeAnalysis

