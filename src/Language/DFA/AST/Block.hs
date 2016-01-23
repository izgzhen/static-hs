module Language.DFA.AST.Block where

-- Basic Block of Stmt

import Language.DFA.AST.Stmt
import Language.DFA.Core.Label

-- L: Block -> Label is a bijective function
data Block a = BBExp (BExp, a) | BStmt (Stmt a) deriving (Show, Eq, Ord)

type Blocks a = [Block a]

-- blocks inside a statement
blocks :: Label a => Stmt a -> Blocks a
blocks s = case s of
    Assign _ _ _            -> [BStmt s]
    Skip _                  -> [BStmt s]
    Seq s1 s2               -> blocks s1 ++ blocks s2
    IfThenElse bexp s1 s2   -> BBExp bexp : (blocks s1 ++ blocks s2)
    While bexp s            -> BBExp bexp : blocks s

labelOfBlock :: Label a => Block a -> a
labelOfBlock (BBExp (_, l))          = l
labelOfBlock (BStmt (Skip l))        = l
labelOfBlock (BStmt (Assign l _ _))  = l
