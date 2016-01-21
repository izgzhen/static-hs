module AST where

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
          deriving (Show, Eq, Ord)

data BExp = BVar Bool
          | BNot BExp
          | BInfixB BExp BOp BExp
          | BInfixA AExp ROp AExp
          deriving (Show, Eq, Ord)

data BOp = And   | Or                   deriving (Show, Eq, Ord)
data AOp = Plus  | Minus   | Multiply   deriving (Show, Eq, Ord)
data ROp = Equal | Greater | Less       deriving (Show, Eq, Ord)



