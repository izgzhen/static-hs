{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

-- Available Expression Analysis

module AEA where

import AST
import Label
import Iteration

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set hiding (filter, foldr, map)
import qualified Data.Set as S
import Control.Lens

data Solution = Solution {
    _aeEntry :: M.Map Label (Set AExp),
    _aeExit  :: M.Map Label (Set AExp)
} deriving Eq


initSol :: Int -> Solution
initSol nLabels = Solution initial initial
    where
        initial = M.fromList $ zip (map Label [1..nLabels]) $ repeat empty

instance Show Solution where
    show sol@Solution{..} =
        unlines $ "AE Entry" : f _aeEntry ++ ["AE Exit:"] ++ f _aeExit
            where
                f = M.elems . M.mapWithKey
                        (\(Label i) exps -> show i ++ " : " ++ show (toList exps))

makeLenses ''Solution

-- The main algorithm based on chaotic iteration

aea :: Stmt Label -> Solution
aea stmt = chaotic (initSol nLabels) improveSol
    where
        nLabels = length $ labels stmt

        improveSol :: Solution -> Solution
        improveSol = foldr1 (.) $ map with [1..nLabels]
            where
                with :: Int -> (Solution -> Solution)
                with i = let f = aeEntrySingleStep stmt (Label i)
                             g = aeExitSingleStep  stmt (Label i)
                         in  g . f

-- kill and gen functions
kill :: Set AExp -> Block Label -> Set AExp
kill exps (BStmt (Assign l x a)) =
    fromList [ a' | a' <- filter nonTrivial (toList exps)
                  , x `member` fv a' ]
kill _ _ = empty

gen :: Block Label -> Set AExp
gen (BStmt (Assign _ x a)) = fromList [ a' | a' <- filter nonTrivial $ toList (subExpsOf a)
                                           , not (x `member` fv a') ]
gen (BStmt (Skip _))       = empty
gen (BBExp (bexp, _))      = S.filter nonTrivial $ subExpsOf bexp

-- Single step update

aeEntrySingleStep :: Stmt Label -> Label -> Solution -> Solution
aeEntrySingleStep stmt l sol
    | initLabel stmt == l = sol
    | otherwise           = aeEntry %~ (M.insert l s) $ sol
        where s = foldr1 intersection
                         [ unsafeLookup l' (_aeExit sol) | (l', l'') <- toList $ flow stmt
                                                         , l == l'' ]

aeExitSingleStep :: Stmt Label -> Label -> Solution -> Solution
aeExitSingleStep stmt l sol =
    let exps        = getExps stmt
        bs          = blocks stmt
        block       = head $ filter (\b -> labelOfBlock b == l) bs
        enteredExps = unsafeLookup l (_aeEntry sol)
        killedExps  = kill exps block
        genExps     = gen block
        s           = (enteredExps \\ killedExps) `union` genExps
    in  aeExit %~ (M.insert l s) $ sol

-- Misc

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

