{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

module AST where

import Data.Set (Set, singleton)

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

nonTrivial :: AExp -> Bool
nonTrivial (AInfix _ _ _) = True
nonTrivial _ = False

-- Some abstract shit

class Recursive a e where
    recursive :: Container m e => a -> m e

class Monoid (m e) => Container m e where
    single :: e -> m e

class Collect a where
    collect :: Monoid m => (a -> m) -> a -> m

instance Recursive AExp a => Recursive BExp a where
    recursive = collect f
        where
            f (BInfixA a1 _ a2) = recursive a1 `mappend` recursive a2
            f _ = mempty

instance Collect a => Recursive a a where
    recursive = collect single

instance Recursive AExp Name where
    recursive = collect f
        where
            f (AVar x) = single x
            f _        = mempty

instance Recursive (Stmt a) AExp where
    recursive = collect f
        where
            f :: Container m AExp => Stmt a -> m AExp
            f (Assign _ _ aexp)          = recursive aexp
            f (IfThenElse (bexp, _) _ _) = recursive bexp
            f (While (bexp, _) _)        = recursive bexp
            f _                          = mempty

instance Recursive (Stmt a) Name where
    recursive = collect f
        where
            f :: Container m Name => Stmt a -> m Name
            f (Assign _ x aexp)          = single x `mappend` recursive aexp
            f (IfThenElse (bexp, _) _ _) = recursive bexp
            f (While (bexp, _) _)        = recursive bexp
            f _                          = mempty

instance Collect (Stmt a) where
    collect f s = f s `mappend` case s of
        Assign _ _ _       -> mempty
        Skip _             -> mempty
        Seq s1 s2          -> collect' s1 `mappend` collect' s2
        IfThenElse _ s1 s2 -> collect' s1 `mappend` collect' s2
        While _ s          -> collect' s
        where
            collect' = collect f

instance Collect BExp where
    collect f a = f a `mappend` case a of
        BLit _          -> mempty
        BNot bexp       -> collect' bexp
        BInfixB b1 _ b2 -> collect' b1 `mappend` collect' b2
        BInfixA a1 _ a2 -> mempty
        where
            collect' = collect f

instance Collect AExp where
    collect f a = f a `mappend` case a of
        AVar _         -> mempty
        ANum _         -> mempty
        AInfix a1 _ a2 -> collect' a1 `mappend` collect' a2
            where
                collect' = collect f

instance Ord a => Container Set a where
    single = singleton
