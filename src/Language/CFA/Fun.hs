-- Fun language

module Language.CFA.Fun where

import Data.Set

newtype Label = Label { unLabel :: Int } deriving (Eq, Show, Ord)

type Name = String

data Expr = Expr Label Term deriving (Eq, Show, Ord)

data Term = TConst Int
          | TVar Name
          | TFn Name Expr
          | TFun Name Name Expr
          | TApp Expr Expr
          | TIf Expr Expr Expr
          | TLet Name Expr Expr
          | TOp Expr Op Expr
          deriving (Show, Eq, Ord)

data Op = OPlus | OMinus deriving (Show, Eq, Ord)


class Fv a where
    fv :: a -> Set Name

instance Fv Term where
    fv (TConst _)     = empty
    fv (TVar x)       = singleton x
    fv (TApp e1 e2)   = fv e1 `union` fv e2
    fv (TFn x e)      = fv e \\ singleton x
    fv (TFun f x e)   = fv e \\ fromList [f, x]
    fv (TLet x e1 e2) = fv e1 `union` (fv e2 \\ singleton x)
    fv (TIf e1 e2 e3) = fv e1 \\ fv e2 \\ fv e3
    fv (TOp e1 _ e2)  = fv e1 `union` fv e2

instance Fv Expr where
    fv (Expr _ t) = fv t

class SubTerm a where
    subTermsOf :: a -> Set Term

instance SubTerm Term where
    subTermsOf t = singleton t `union` subTermsOf' t
        where
            subTermsOf' (TConst _)     = empty
            subTermsOf' (TVar _)       = empty
            subTermsOf' (TApp e1 e2)   = subTermsOf e1 `union` subTermsOf e2
            subTermsOf' (TFn _ e)      = subTermsOf e
            subTermsOf' (TFun _ _ e)   = subTermsOf e
            subTermsOf' (TLet _ e1 e2) = subTermsOf e1 `union` subTermsOf e2
            subTermsOf' (TIf e1 e2 e3) = subTermsOf e1 `union` subTermsOf e2 `union` subTermsOf e3
            subTermsOf' (TOp e1 _ e2)  = subTermsOf e1 `union` subTermsOf e2

instance SubTerm Expr where
    subTermsOf (Expr _ t) = subTermsOf t

