{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

module AST where

import Data.Set hiding (foldr)

type Name = String

data Stmt a = Assign a Name AExp
            | Skip a
            | Seq (Stmt a) (Stmt a)
            | IfThenElse (BExp, a) (Stmt a) (Stmt a)
            | While (BExp, a) (Stmt a)
            deriving (Show, Eq, Ord)

data AExp = AVar Name
          | ANum Int
          | AInfix AExp AOp AExp
          deriving (Eq, Ord)

data BExp = BLit Bool
          | BNot BExp
          | BInfixB BExp BOp BExp
          | BInfixA AExp ROp AExp
          deriving (Eq, Ord)

data BOp = And   | Or                   deriving (Eq, Ord)
data AOp = Plus  | Minus   | Multiply   deriving (Eq, Ord)
data ROp = Equal | Greater | Less       deriving (Eq, Ord)


instance Show AExp where
    show (AVar x) = x
    show (ANum i) = show i
    show (AInfix a1 op a2) = showInfix a1 op a2

instance Show BExp where
    show (BLit b) = show b
    show (BNot b) = "~" ++ show b
    show (BInfixB b1 op b2) = showInfix b1 op b2
    show (BInfixA a1 op a2) = showInfix a1 op a2

instance Show BOp where
    show And = "&&"
    show Or  = "||"

instance Show AOp where
    show Plus     = "+"
    show Minus    = "-"
    show Multiply = "*"

instance Show ROp where
    show Equal    = "="
    show Greater  = ">"
    show Less     = "<"


showInfix :: (Show a, Show b, Show c) => a -> b -> c -> String
showInfix a1 op a2 = "(" ++ show a1 ++ " " ++ show op ++ " " ++ show a2 ++ ")"

-- Free variable
class FV a where
    fv :: a -> Set Name

instance FV AExp where
    fv (AVar x) = singleton x
    fv (ANum _) = empty
    fv (AInfix e1 _ e2) = fv e1 `union` fv e2


-- Get sub arith expressions of statement
-- Used by AEA

nonTrivial :: AExp -> Bool
nonTrivial (AInfix _ _ _) = True
nonTrivial _ = False

class Recursive a e where
    recursive :: Container m e => a -> m e

class Monoid (m e) => Container m e where
    single :: e -> m e


instance Recursive AExp a => Recursive BExp a where
    recursive (BLit _)          = mempty
    recursive (BNot bexp)       = recursive bexp
    recursive (BInfixB b1 _ b2) = recursive b1 `mappend` recursive b2
    recursive (BInfixA a1 _ a2) = recursive a1 `mappend` recursive a2

instance Recursive AExp AExp where
    recursive a@(AVar _)         = single a
    recursive a@(ANum _)         = single a
    recursive a@(AInfix a1 _ a2) = mconcat [single a, recursive a1, recursive a2]

instance Recursive AExp Name where
    recursive (AVar x)         = single x
    recursive (ANum _)         = mempty
    recursive (AInfix a1 _ a2) = mconcat [recursive a1, recursive a2]

instance Recursive (Stmt a) AExp where
    recursive = collectStmt f
        where
            f :: Container m AExp => Stmt a -> m AExp
            f (Assign _ _ aexp)          = recursive aexp
            f (IfThenElse (bexp, _) _ _) = recursive bexp
            f (While (bexp, _) _)        = recursive bexp
            f _                          = mempty

instance Recursive (Stmt a) Name where
    recursive = collectStmt f
        where
            f :: Container m Name => Stmt a -> m Name
            f (Assign _ x aexp)          = single x `mappend` recursive aexp
            f (IfThenElse (bexp, _) _ _) = recursive bexp
            f (While (bexp, _) _)        = recursive bexp
            f _                          = mempty

collectStmt :: Monoid m => (Stmt a -> m) -> Stmt a -> m
collectStmt f s = case s of
    (Assign _ _ _)       -> f s
    (Skip _)             -> mempty
    (Seq s1 s2)          -> collectStmt' s1 `mappend` collectStmt' s2
    (IfThenElse _ s1 s2) -> mconcat [f s, collectStmt' s1, collectStmt' s2]
    (While _ s)          -> f s `mappend` collectStmt' s
    where
        collectStmt' = collectStmt f

instance Ord a => Container Set a where
    single = singleton
