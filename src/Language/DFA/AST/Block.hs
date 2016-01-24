module Language.DFA.AST.Block where

-- Basic Block of Stmt

import Language.DFA.AST.Stmt
import Language.DFA.Core.Label
import Data.Map

-- L: Block -> Label is a bijective function
data Block a = BBExp (BExp, a) | BStmt (Stmt a) deriving (Show, Eq, Ord)

-- blocks inside a statement
blocks :: Label a => Stmt a -> Map a (Block a)
blocks s = case s of
    Assign l _ _            -> singleton l (BStmt s)
    Skip l                  -> singleton l (BStmt s)
    Seq s1 s2               -> blocks s1 `union` blocks s2
    IfThenElse (bexp, l) s1 s2 ->
        insert l (BBExp (bexp, l)) (blocks s1 `union` blocks s2)
    While (bexp, l) s       -> insert l (BBExp (bexp, l)) (blocks s)

labelOfBlock :: Label a => Block a -> a
labelOfBlock (BBExp (_, l))          = l
labelOfBlock (BStmt (Skip l))        = l
labelOfBlock (BStmt (Assign l _ _))  = l
