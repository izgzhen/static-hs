{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RDAPackage where

import Mono
import AST
import Label
import Common

import qualified Data.Set  as S
import qualified Data.List as L
import qualified Data.Map  as M
import Control.Lens
import Iteration

type RDProperty a = [(Name, Maybe a)]

type RDSolution a = Solution a (RDProperty a)

rdLattice :: Label a => Lattice (RDProperty a)
rdLattice = Lattice {
  _lessThen = L.isSubsequenceOf
, _meet     = L.union
, _bottom   = []
}

rdAnalysis :: Label a => Analysis Stmt (RDProperty a) a
rdAnalysis = Analysis {
  _lattice      = rdLattice
, _extermals    = S.singleton . initLabel
, _initSol      = rdInitSol
, _flow         = flow
, _transfer     = rdTransfer
, _labels       = labels
}

rdInitSol :: Label a => Stmt a -> Solution a (RDProperty a)
rdInitSol stmt = Solution initial initial
    where
        names   = S.toList $ fv stmt
        initial = M.fromList $ zip (S.toList $ labels stmt) $
                                   repeat (zip names (repeat Nothing))

rdTransfer :: Label a => Stmt a -> a -> Solution a (RDProperty a) -> Solution a (RDProperty a)
rdTransfer stmt l sol =
    let block       = head $ filter (\b -> labelOfBlock b == l) $ blocks stmt
        fromEntry   = unsafeLookup l (_entry sol)
        s           = (fromEntry L.\\ kill stmt block) ++ gen block
    in  exit %~ (M.insert l s) $ sol
    where
        -- kill and gen functions
        kill :: Stmt a -> Block a -> RDProperty a
        kill stmt (BStmt (Assign l x a)) =
            (x, Nothing) : [ (x, Just l') | Assign l' x' _ <- collectAssignments stmt
                                          , x' == x ]
        kill _ _ = []

        gen :: Block a -> RDProperty a
        gen (BStmt (Assign l x _)) = [(x, Just l)]
        gen _ = []

rda :: (Label a, Eq (Solution a (RDProperty a)), Show (Solution a (RDProperty a))) =>
       DebugOption -> Stmt a -> Solution a (RDProperty a)
rda opt = analyze opt rdAnalysis

-- Misc

fv :: Recursive a Name => a -> S.Set Name
fv = recursive

instance Recursive (Stmt a) (Stmt a) where
    recursive = collect f
        where
            f s@(Assign _ _ _) = single s
            f _ = mempty

collectAssignments :: Stmt a -> [Stmt a]
collectAssignments = recursive
