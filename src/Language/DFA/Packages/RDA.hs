{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

-- Reaching Definition Analysis Package

module Language.DFA.Packages.RDA where

import Language.DFA.Core.Mono
import Language.DFA.AST
import Language.DFA.AST.Recursive
import Language.DFA.AST.Label
import Language.DFA.AST.Block
import Language.DFA.Core.Label
import Language.DFA.Common

import qualified Data.Set  as S
import qualified Data.List as L
import qualified Data.Map  as M
import Control.Lens

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
  _lattice      = \_ -> rdLattice
, _extermals    = S.singleton . initLabel
, _initSol      = rdInitSol
, _flow         = flow
, _transfer     = rdTransfer
, _labels       = labels
, _direction    = Forward
}

rdInitSol :: Label a => Stmt a -> RDSolution a
rdInitSol stmt = Solution initial initial
    where
        names   = S.toList $ fv stmt
        initial = M.fromList $ zip (S.toList $ labels stmt) $
                                   repeat (zip names (repeat Nothing))

rdTransfer :: Label a => Stmt a -> a -> RDSolution a -> RDSolution a
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

rda :: (Label a, Eq (RDSolution a), Show (RDSolution a)) =>
       DebugOption -> Stmt a -> Solution a (RDProperty a)
rda opt = analyze opt rdAnalysis

-- Misc

instance Recursive (Stmt a) (Stmt a) where
    recursive = collect f
        where
            f s@(Assign _ _ _) = single s
            f _ = mempty

collectAssignments :: Stmt a -> [Stmt a]
collectAssignments = recursive
