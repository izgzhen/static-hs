{-# LANGUAGE TemplateHaskell, RecordWildCards,
             MultiParamTypeClasses, FlexibleContexts #-}

-- Very Busy Expressions Analysis

module VBEA where

import AST
import Label
import Iteration

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set hiding (filter, foldr, map)
import qualified Data.Set as S
import Control.Lens

data Solution = Solution {
    _vbeEntry :: M.Map Label (Set AExp),
    _vbeExit  :: M.Map Label (Set AExp)
} deriving Eq


initSol :: Int -> Solution
initSol nLabels = Solution initial initial
    where
        initial = M.fromList $ zip (map Label [1..nLabels]) $ repeat empty

instance Show Solution where
    show sol@Solution{..} =
        unlines $ "VBE Entry" : f _vbeEntry ++ ["VBE Exit:"] ++ f _vbeExit
            where
                f = M.elems . M.mapWithKey
                        (\(Label i) exps -> show i ++ " : " ++ show (toList exps))

makeLenses ''Solution

-- The main algorithm based on chaotic iteration

vbea :: Stmt Label -> Solution
vbea stmt = chaotic (initSol nLabels) improveSol
    where
        nLabels = length $ labels stmt

        improveSol :: Solution -> Solution
        improveSol = foldr1 (.) $ map with $ reverse [1..nLabels]
            where
                with :: Int -> (Solution -> Solution)
                with i = let f = vbeEntrySingleStep stmt (Label i)
                             g = vbeExitSingleStep  stmt (Label i)
                         in  f . g

-- kill and gen functions
kill :: Set AExp -> Block Label -> Set AExp
kill exps (BStmt (Assign l x a)) =
    fromList [ a' | a' <- filter nonTrivial (toList exps)
                  , x `member` fv a' ]
kill _ _ = empty

gen :: Block Label -> Set AExp
gen (BStmt (Assign _ x a)) = S.filter nonTrivial (subAExps a)
gen (BStmt (Skip _))       = empty
gen (BBExp (bexp, _))      = S.filter nonTrivial (subAExps bexp)

-- Single step update

vbeEntrySingleStep :: Stmt Label -> Label -> Solution -> Solution
vbeEntrySingleStep stmt l sol =
    let exps        = subAExps stmt
        block       = head $ filter (\b -> labelOfBlock b == l) $ blocks stmt
        enteredExps = unsafeLookup l (_vbeExit sol)
        s           = (enteredExps \\ kill exps block) `union` gen block
    in  vbeEntry %~ (M.insert l s) $ sol

vbeExitSingleStep :: Stmt Label -> Label -> Solution -> Solution
vbeExitSingleStep stmt l sol
    | l `member` finalLabels stmt = sol
    | otherwise           = vbeExit %~ (M.insert l s) $ sol
        where s = let ss = [ unsafeLookup l' (_vbeEntry sol) | (l'', l') <- toList $ flow stmt
                                                             , l == l'' ]
                  in  case ss of
                        []  -> empty
                        ss' -> foldr1 intersection ss'


-- Misc

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

fv :: Recursive a Name => a -> Set Name
fv = recursive

subAExps :: Recursive a AExp => a -> Set AExp
subAExps = recursive


