module AST where

import Data.Set

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
fv :: AExp -> Set Name
fv (AVar x) = singleton x
fv (ANum _) = empty
fv (AInfix e1 _ e2) = fv e1 `union` fv e2

-- Get sub arith expressions of statement
-- Used by AEA

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
    subExpsOf a@(AVar _)         = singleton a
    subExpsOf a@(ANum _)         = singleton a
    subExpsOf a@(AInfix a1 _ a2) = singleton a `union` subExpsOf a1 `union` subExpsOf a2
