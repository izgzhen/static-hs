{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

-- Resursive Instance of Stmt

module Language.DFA.AST.Recursive where

import Language.DFA.Common
import Language.DFA.AST.Def

fv :: (Container m Name, Recursive a Name) => a -> m Name
fv = recursive

subAExps :: (Container m AExp, Recursive a AExp) => a -> m AExp
subAExps = recursive

instance Recursive AExp a => Recursive BExp a where
    recursive = collect f
        where
            f (BInfixA a1 _ a2) = recursive a1 `mappend` recursive a2
            f _ = mempty

instance Recursive AExp AExp where
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
