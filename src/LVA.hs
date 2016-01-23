{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- Live Variables Analysis

module LVA where

import AST
import Label
import Mono
import Common

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

lvAnalysis :: Label a => Analysis Stmt LVProperty a
lvAnalysis = Analysis {
  _lattice   = \_ -> lvLattice
, _extermals = finalLabels
, _initSol   = lvInitSol
, _flow      = reverseFlow
, _transfer  = lvTransfer
, _labels    = labels
, _direction = Backward
}

lvInitSol :: Label a => Stmt a -> LVSolution a
lvInitSol stmt = Solution initial initial
    where
        initial = M.fromList $ zip (toList $ labels stmt) $ repeat empty

lvTransfer :: Label a => Stmt a -> a -> LVSolution a -> LVSolution a
lvTransfer stmt l sol =
    let block       = head $ filter (\b -> labelOfBlock b == l) $ blocks stmt
        fromExit    = unsafeLookup l (_exit sol)
        s           = (fromExit \\ kill block) `union` gen block
    in  entry %~ (M.insert l s) $ sol
    where
        -- kill and gen functions
        kill :: Block a -> LVProperty
        kill (BStmt (Assign l x a)) = singleton x
        kill _                      = empty

        gen :: Block a -> LVProperty
        gen (BStmt (Assign _ x a)) = fv a
        gen (BStmt (Skip _))       = empty
        gen (BBExp (bexp, _))      = fv bexp

lva :: (Label a, Eq (LVSolution a), Show (LVSolution a)) =>
       DebugOption -> Stmt a -> Solution a LVProperty
lva opt = analyze opt lvAnalysis
