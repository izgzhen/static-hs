{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances, UndecidableInstances,
             DataKinds, PolyKinds #-}

-- Resursive Instance of Stmt

module Language.DFA.AST.Recursive where

import Language.DFA.Common
import Language.DFA.AST.Def

data Fv = Fv
data SubAExps = SubAExps

fv :: (Container m Name, Recursive a Name Fv) => a -> m Name
fv = recursive Fv
 
subAExps :: (Container m AExp, Recursive a AExp SubAExps) => a -> m AExp
subAExps = recursive SubAExps

instance Recursive AExp a t => Recursive BExp a t where
    recursive ty = collect f
        where
            f (BInfixA a1 _ a2) = recursive ty a1 `mappend` recursive ty a2
            f _ = mempty

instance Recursive AExp AExp SubAExps where
    recursive _ = collect single

instance Recursive AExp Name Fv where
    recursive _ = collect f
        where
            f (AVar x) = single x
            f _        = mempty

instance Recursive (Stmt a) AExp SubAExps where
    recursive ty = collect f
        where
            f :: Container m AExp => Stmt a -> m AExp
            f (Assign _ _ aexp)          = recursive ty aexp
            f (IfThenElse (bexp, _) _ _) = recursive ty bexp
            f (While (bexp, _) _)        = recursive ty bexp
            f _                          = mempty

instance Recursive (Stmt a) Name Fv where
    recursive ty = collect f
        where
            f :: Container m Name => Stmt a -> m Name
            f (Assign _ x aexp)          = single x `mappend` recursive ty aexp
            f (IfThenElse (bexp, _) _ _) = recursive ty bexp
            f (While (bexp, _) _)        = recursive ty bexp
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
