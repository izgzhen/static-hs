{-# LANGUAGE FlexibleContexts #-}

module RDAPackage where

import Mono
import AST
import Label

import qualified Data.Set  as S
import qualified Data.List as L

type RDProperty a = [(Name, Maybe a)]

type RDSolution a = Solution a (RDProperty a)

rdLattice :: (Ord a, Eq a) => Lattice (RDProperty a)
rdLattice = Lattice {
  lessThen = L.isSubsequenceOf
, meet     = L.union
, bottom   = []
}

rdAnalysis :: (Ord a, Eq a) => Analysis Stmt (RDProperty a) a
rdAnalysis = Analysis {
  _lattice      = rdLattice
, _extermals    = S.singleton . initLabel
, _extermalVal  = \stmt -> zip (S.toList $ fv stmt) $ repeat Nothing
, _flow         = flow
, _transfer     = undefined
}


fv :: Recursive a Name => a -> S.Set Name
fv = recursive
