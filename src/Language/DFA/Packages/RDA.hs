{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

-- Reaching Definition Analysis Package

module Language.DFA.Packages.RDA where

import Language.DFA.Core.Mono
import Language.DFA.AST
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

rdAnalysis :: Label a => Analysis Stmt Block (RDProperty a) a
rdAnalysis = Analysis {
  _lattice      = \_ -> rdLattice
, _extermals    = S.singleton . initLabel
, _initSol      = rdInitSol
, _flow         = flow
, _transfer     = rdTransfer
, _labels       = labels
, _direction    = Forward
, _blocks       = blocks
}

rdInitSol :: Label a => Stmt a -> M.Map a (RDProperty a)
rdInitSol stmt = M.fromList $ zip (S.toList $ labels stmt) $
                                  repeat (zip (S.toList $ fv stmt) (repeat Nothing))

rdTransfer :: Label a => Stmt a -> (Block, a) -> RDProperty a -> RDProperty a
rdTransfer stmt (block, l) entered = (entered L.\\ kill stmt block) ++ gen block
    where
        -- kill and gen functions
        kill stmt (BAssign x a) =
            (x, Nothing) : [ (x, Just l') | Assign l' x' _ <- collectAssignments stmt
                                          , x' == x ]
        kill _ _ = []

        gen (BAssign x _) = [(x, Just l)]
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
