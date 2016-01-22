{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

-- Available Expression Analysis

module AEA where

import AST
import Label
import Iteration

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set hiding (filter, foldr, map)
import Control.Lens

-- The main algorithm based on chaotic iteration
data Solution = Solution {
    _aeEntry :: M.Map Label (Set AExp),
    _aeExit  :: M.Map Label (Set AExp)
} deriving Eq

instance Show Solution where
    show sol@Solution{..} =
        unlines $ "AE Entry" : f _aeEntry ++ ["AE Exit:"] ++ f _aeExit
            where
                f = M.elems . M.mapWithKey
                        (\(Label i) exps -> show i ++ " : " ++ show (toList exps))

makeLenses ''Solution

-- Free variable
fv :: AExp -> Set Name
fv (AVar x) = singleton x
fv (ANum _) = empty
fv (AInfix e1 _ e2) = fv e1 `union` fv e2

-- kill and gen functions
kill :: Set AExp -> Block Label -> Set AExp
kill exps (BStmt (Assign l x a)) =
    fromList [ a' | a' <- filter nonTrivial (toList exps)
                  , x `member` fv a' ]
kill _ _ = empty

gen :: Block Label -> Set AExp
gen (BStmt (Assign _ x a)) = fromList [ a' | a' <- toList (subExpsOf a), not (x `member` fv a') ]
gen (BStmt (Skip _))       = empty
gen (BBExp (bexp, _))      = subExpsOf bexp

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
        block       = head $ filter (blockHasLabel l) bs
        enteredExps = unsafeLookup l (_aeEntry sol)
        killedExps  = kill exps block
        genExps     = gen block
        s           = (enteredExps \\ killedExps) `union` genExps
    in  aeExit %~ (M.insert l s) $ sol


initSol :: Int -> Solution
initSol nLabels = Solution initial initial
    where
        initial = M.fromList $ zip (map Label [1..nLabels]) $ repeat empty

updateEntry :: Solution -> Label -> Set AExp -> Solution
updateEntry sol l s = aeEntry %~ (M.insert l s) $ sol

aea :: Stmt Label -> Solution
aea stmt = chaotic (initSol nLabels) improveSol
    where
        nLabels = countLabels stmt

        improveSol :: Solution -> Solution
        improveSol = foldr1 (.) $ map with [1..nLabels]
            where
                with :: Int -> (Solution -> Solution)
                with i = let f = aeEntrySingleStep stmt (Label i)
                             g = aeExitSingleStep  stmt (Label i)
                         in  g . f

countLabels :: Stmt Label -> Int
countLabels (Assign _ _ _) = 1
countLabels (Skip _)       = 1
countLabels (Seq s1 s2)    = countLabels s1 + countLabels s2
countLabels (IfThenElse _ s1 s2) = 1 + countLabels s1 + countLabels s2
countLabels (While _ s)    = 1 + countLabels s

getExps :: Stmt a -> Set AExp
getExps (Assign _ _ e)      = singleton e `union` subExpsOf e
getExps (Skip _)            = empty
getExps (Seq s1 s2)         = getExps s1 `union` getExps s2
getExps (IfThenElse (bexp, _) s1 s2) =
    subExpsOf bexp `union` getExps s1 `union` getExps s2
getExps (While (bexp, _) s) = subExpsOf bexp `union` getExps s

nonTrivial :: AExp -> Bool
nonTrivial (AInfix _ _ _) = True
nonTrivial _ = False

class ToAExps a where
    subExpsOf :: a -> Set AExp

instance ToAExps BExp where
    subExpsOf (BLit _)          = empty
    subExpsOf (BNot bexp)       = subExpsOf bexp
    subExpsOf (BInfixB b1 _ b2) = subExpsOf b1 `union` subExpsOf b2
    subExpsOf (BInfixA a1 _ a2) = subExpsOf a1 `union` subExpsOf a2

instance ToAExps AExp where
    subExpsOf (AVar _)           = empty
    subExpsOf (ANum _)           = empty
    subExpsOf a@(AInfix a1 _ a2) = singleton a `union` subExpsOf a1 `union` subExpsOf a2


unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

