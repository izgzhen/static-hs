{-# LANGUAGE TemplateHaskell, RecordWildCards,
             MultiParamTypeClasses, FlexibleContexts #-}

-- Live Variables Analysis

module LVA where

import AST
import Label
import Iteration

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set hiding (filter, foldr, map)
import qualified Data.Set as S
import Control.Lens

data Solution = Solution {
    _lvEntry :: M.Map Label (Set Name),
    _lvExit  :: M.Map Label (Set Name)
} deriving Eq


initSol :: Int -> Solution
initSol nLabels = Solution initial initial
    where
        initial = M.fromList $ zip (map Label [1..nLabels]) $ repeat empty

instance Show Solution where
    show sol@Solution{..} =
        unlines $ "LV Entry" : f _lvEntry ++ ["LV Exit:"] ++ f _lvExit
            where
                f = M.elems . M.mapWithKey
                        (\(Label i) exps -> show i ++ " : " ++ show (toList exps))

makeLenses ''Solution

-- The main algorithm based on chaotic iteration

lva :: Stmt Label -> Solution
lva stmt = chaotic (initSol nLabels) improveSol
    where
        nLabels = length $ labels stmt

        improveSol :: Solution -> Solution
        improveSol = foldr1 (.) $ map with $ reverse [1..nLabels]
            where
                with :: Int -> (Solution -> Solution)
                with i = let f = lvEntrySingleStep stmt (Label i)
                             g = lvExitSingleStep  stmt (Label i)
                         in  f . g

-- kill and gen functions
kill :: Block Label -> Set Name
kill (BStmt (Assign l x a)) = singleton x
kill _                      = empty

gen :: Block Label -> Set Name
gen (BStmt (Assign _ x a)) = fv a
gen (BStmt (Skip _))       = empty
gen (BBExp (bexp, _))      = fv bexp

-- Single step update

lvEntrySingleStep :: Stmt Label -> Label -> Solution -> Solution
lvEntrySingleStep stmt l sol =
    let block       = head $ filter (\b -> labelOfBlock b == l) $ blocks stmt
        fromExit    = unsafeLookup l (_lvExit sol)
        s           = (fromExit \\ kill block) `union` gen block
    in  lvEntry %~ (M.insert l s) $ sol

lvExitSingleStep :: Stmt Label -> Label -> Solution -> Solution
lvExitSingleStep stmt l sol
    | l `member` finalLabels stmt = sol
    | otherwise           = lvExit %~ (M.insert l s) $ sol
        where s = foldr union empty
                        [ unsafeLookup l' (_lvEntry sol) | (l'', l') <- toList $ flow stmt
                                                         , l == l'' ]

-- Misc

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

fv :: Recursive a Name => a -> Set Name
fv = recursive

subAExps :: Recursive a AExp => a -> Set AExp
subAExps = recursive



debugExample5Sol :: Solution
debugExample5Sol = Solution {
      _lvEntry = M.fromList
        [ (Label 1, fromList [])
        , (Label 2, fromList [])
        , (Label 3, fromList ["y"])
        , (Label 4, fromList ["x","y"])
        , (Label 5, fromList ["y"])
        , (Label 6, fromList ["y"])
        , (Label 7, fromList ["z"]) ]
    , _lvExit = M.fromList
        [ (Label 1, fromList [])
        , (Label 2, fromList ["y"])
        , (Label 3, fromList ["x","y"])
        , (Label 4, fromList ["y"])
        , (Label 5, fromList ["z"])
        , (Label 6, fromList ["z"])
        , (Label 7, fromList []) ]
}

