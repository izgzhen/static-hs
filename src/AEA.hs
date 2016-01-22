-- Available Expression Analysis

module AEA where

import AST
import Label

import Data.Set hiding (filter, foldr)

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

aeEntry :: Stmt Label -> Label -> Set AExp
aeEntry stmt l
    | initLabel stmt == l = empty
    | otherwise           = foldr union empty [ aeExit stmt l' | (l', _) <- toList $ flow stmt ]

aeExit :: Stmt Label -> Label -> Set AExp
aeExit stmt l = let exps        = getExps stmt
                    bs          = blocks stmt
                    block       = head $ filter (blockHasLabel l) bs
                    enteredExps = aeEntry stmt l
                    killedExps  = kill exps block
                    genExps     = gen block
                in  (enteredExps \\ killedExps) `union` genExps

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


