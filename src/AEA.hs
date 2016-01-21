-- Available Expression Analysis

module AEA where

import AST
import Label

import Data.Set hiding (filter)

-- Free variable
fv :: AExp -> Set Name
fv (AVar x) = singleton x
fv (ANum _) = empty
fv (AInfix e1 _ e2) = fv e1 `union` fv e2

-- kill and gen functions
kill :: [AExp] -> Block Label -> Set AExp
kill exps (BStmt (Assign l x a)) = fromList [ a' | a' <- filter nonTrivial exps, x `member` fv a' ]
kill _ _ = empty

gen :: Block Label -> Set AExp
gen (BStmt (Assign _ x a)) = fromList [ a' | a' <- toList (subExpsOf a), not (x `member` fv a') ]
gen (BStmt (Skip _))       = empty
gen (BBExp (bexp, _))      = subExpsOf bexp

nonTrivial :: AExp -> Bool
nonTrivial (AInfix _ _ _) = False
nonTrivial _ = True


class ToAExps a where
    subExpsOf :: a -> Set AExp

instance ToAExps BExp where
    subExpsOf (BVar _) = empty
    subExpsOf (BNot bexp) = subExpsOf bexp
    subExpsOf (BInfixB b1 _ b2) = subExpsOf b1 `union` subExpsOf b2
    subExpsOf (BInfixA a1 _ a2) = subExpsOf a1 `union` subExpsOf a2

instance ToAExps AExp where
    subExpsOf (AVar _) = empty
    subExpsOf (ANum _) = empty
    subExpsOf (AInfix a1 _ a2) = subExpsOf a1 `union` subExpsOf a2


