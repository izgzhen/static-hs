{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- Available Expression Analysis

module Language.DFA.Packages.AEA where

import Language.DFA.Core.Mono
import Language.DFA.AST
import Language.DFA.Core.Label
import Language.DFA.Common

import Data.Set hiding (filter)
import qualified Data.Set as S
import qualified Data.Map  as M
import Control.Lens

type AEProperty = Set AExp

type AESolution a = Solution a AEProperty

aeLattice :: Label a => Stmt a -> Lattice AEProperty
aeLattice stmt = Lattice {
  _lessThen = flip isSubsetOf -- isSupersetOf
, _meet     = intersection
, _bottom   = S.filter nonTrivial $ subAExps stmt
}

aeAnalysis :: Label a => Analysis Stmt Block AEProperty a
aeAnalysis = Analysis {
  _lattice      = aeLattice
, _extermals    = singleton . initLabel
, _initSol      = aeInitSol
, _flow         = flow
, _transfer     = aeTransfer
, _labels       = labels
, _direction    = Forward
, _labelOfBlock = labelOfBlock
, _blocks       = blocks
}

aeInitSol :: Label a => Stmt a -> AESolution a
aeInitSol stmt = Solution initial initial
    where
        initial = M.fromList $ zip (toList $ labels stmt) $ repeat empty

aeTransfer :: Label a => Stmt a -> Block a -> AEProperty -> AEProperty
aeTransfer stmt block entered = entered \\ kill (subAExps stmt) block `union` gen block
    where
        -- kill and gen functions
        kill :: Set AExp -> Block a -> AEProperty
        kill exps (BStmt (Assign l x a)) =
            fromList [ a' | a' <- filter nonTrivial (toList exps)
                          , x `member` fv a' ]
        kill _ _ = empty

        gen :: Block a -> AEProperty
        gen (BStmt (Assign _ x a)) = fromList [ a' | a' <- filter nonTrivial $ toList (subAExps a)
                                                   , not (x `member` fv a') ]
        gen (BStmt (Skip _))       = empty
        gen (BBExp (bexp, _))      = S.filter nonTrivial $ subAExps bexp

aea :: (Label a, Eq (AESolution a), Show (AESolution a)) =>
       DebugOption -> Stmt a -> Solution a AEProperty
aea opt = analyze opt aeAnalysis

