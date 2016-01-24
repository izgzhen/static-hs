{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, LambdaCase #-}

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

type AEProperty = Maybe (Set AExp)

aeMeet :: AEProperty -> AEProperty -> AEProperty
aeMeet Nothing s = s
aeMeet s Nothing = s
aeMeet (Just s1) (Just s2) = Just $ s1 `intersection` s2

aeLessThen :: AEProperty -> AEProperty -> Bool
aeLessThen Nothing _ = True
aeLessThen (Just s1) (Just s2) = s2 `isSubsetOf` s1
aeLessThen _ _ = False

type AESolution a = Solution a AEProperty

aeLattice :: Label a => Stmt a -> Lattice AEProperty
aeLattice stmt = Lattice {
  _lessThen = aeLessThen
, _meet     = aeMeet
, _bottom   = Just $ S.filter nonTrivial $ subAExps stmt
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
, _blocks       = blocks
}

aeInitSol :: Label a => Stmt a -> M.Map a AEProperty
aeInitSol stmt = M.fromList $ zip (toList $ labels stmt) $ repeat Nothing

aeTransfer :: Label a => Stmt a -> Block a -> AEProperty -> AEProperty
aeTransfer stmt block = \case
    Nothing      -> Just $ gen block \\ kill (subAExps stmt) block
    Just entered -> Just $ entered \\ kill (subAExps stmt) block `union` gen block
    where
        -- kill and gen functions
        kill :: Set AExp -> Block a -> Set AExp
        kill exps (BStmt (Assign l x a)) =
            fromList [ a' | a' <- filter nonTrivial (toList exps)
                          , x `member` fv a' ]
        kill _ _ = empty

        gen :: Block a -> Set AExp
        gen (BStmt (Assign _ x a)) = fromList [ a' | a' <- filter nonTrivial $ toList (subAExps a)
                                                   , not (x `member` fv a') ]
        gen (BStmt (Skip _))       = empty
        gen (BBExp (bexp, _))      = S.filter nonTrivial $ subAExps bexp

aea :: (Label a, Eq (AESolution a), Show a) =>
       DebugOption -> Stmt a -> Solution a AEProperty
aea opt = analyze opt aeAnalysis

aea' :: (Label a, Eq (AESolution a), Show a) =>
       DebugOption -> Stmt a -> Solution a AEProperty
aea' opt = analyze' opt aeAnalysis


