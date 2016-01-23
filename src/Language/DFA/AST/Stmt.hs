module Language.DFA.AST.Stmt where

-- Stmt AST definition

type Name = String

data Stmt a = Assign a Name AExp
            | Skip a
            | Seq (Stmt a) (Stmt a)
            | IfThenElse (BExp, a) (Stmt a) (Stmt a)
            | While (BExp, a) (Stmt a)
            deriving (Eq, Ord)

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

-- Printer Section

instance Show a => Show (Stmt a) where
    show (Assign l x e) = labelPrint l $ x ++ " := " ++ show e
    show (Skip l)       = labelPrint l "skip"
    show (Seq s1 s2)    = show s1 ++ "\n" ++ show s2
    show (IfThenElse (bexp, l) s1 s2) =
        "if " ++ labelPrint l (show bexp) ++ " {\n" ++ show s1 ++ "\n} else {\n" ++ show s2 ++ "\n}"
    show (While (bexp, l) s) = "while " ++ labelPrint l (show bexp) ++ "{\n" ++ show s ++ "\n}"

labelPrint :: Show a => a -> String -> String
labelPrint l s = "[" ++ s ++ "]^" ++ show l

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

-- Utilities

nonTrivial :: AExp -> Bool
nonTrivial (AInfix _ _ _) = True
nonTrivial _ = False


applyAOp :: AOp -> Int -> Int -> Int
applyAOp op i1 i2 = case op of
    Plus     -> i1 + i2
    Minus    -> i1 - i2
    Multiply -> i1 * i2

