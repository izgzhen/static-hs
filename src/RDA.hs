{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

-- Reaching Definitions Analysis

module RDA where

import AST
import Label
import Iteration

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set hiding (filter, foldr, map)
import qualified Data.Set as S
import Control.Lens

data Solution = Solution {
    _rdEntry :: M.Map Label [(Name, Maybe Label)],
    _rdExit  :: M.Map Label [(Name, Maybe Label)]
} deriving Eq


initSol :: [Name] -> Int -> Solution
initSol names nLabels = Solution initial initial
    where
        initial = M.fromList $ zip (map Label [1..nLabels]) $
                                   repeat (zip names (repeat Nothing))

instance Show Solution where
    show sol@Solution{..} =
        unlines $ "RD Entry" : f _rdEntry ++ ["RD Exit:"] ++ f _rdExit
            where
                f = M.elems . M.mapWithKey
                        (\(Label i) ps -> show i ++ " : " ++ map g ps)

                g (x, Nothing)        = "(" ++ x ++ ", ?)"
                g (x, Just (Label i)) = "(" ++ x ++ ", " ++ show i ++ ")"

makeLenses ''Solution

-- The main algorithm based on chaotic iteration

rda :: Stmt Label -> Solution
rda stmt = chaotic (initSol names nLabels) improveSol
    where
        nLabels = length $ labels stmt
        names   = fv stmt

        improveSol :: Solution -> Solution
        improveSol = foldr1 (.) $ map with [1..nLabels]
            where
                with :: Int -> (Solution -> Solution)
                with i = let f = aeEntrySingleStep stmt (Label i)
                             g = aeExitSingleStep  stmt (Label i)
                         in  g . f

-- kill and gen functions
kill :: Stmt Label -> Block Label -> [(Name, Maybe Label)]
kill stmt (BStmt (Assign l x a)) =
    (x, Nothing) : [ (x, l') | AST l' x' _ <- collectAssignments stmt
                             , x' == x ]
kill _ _ = []

gen :: Block Label -> [(Name, Maybe Label)]
gen (BStmt (Assign l x _)) = [(x, l)]
gen _ = []

-- Single step update

rdEntrySingleStep :: Stmt Label -> Label -> Solution -> Solution
rdEntrySingleStep stmt l sol
    | initLabel stmt == l = sol
    | otherwise           = rdEntry %~ (M.insert l s) $ sol
        where s = foldr1 intersection
                         [ unsafeLookup l' (_rdExit sol) | (l', l'') <- toList $ flow stmt
                                                         , l == l'' ]

rdExitSingleStep :: Stmt Label -> Label -> Solution -> Solution
rdExitSingleStep stmt l sol =
    let exps        = getExps stmt
        bs          = blocks stmt
        block       = head $ filter (\b -> labelOfBlock b == l) bs
        enteredExps = unsafeLookup l (_rdEntry sol)
        killedExps  = kill exps block
        genExps     = gen block
        s           = (enteredExps \\ killedExps) `union` genExps
    in  rdExit %~ (M.insert l s) $ sol

-- Misc

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

