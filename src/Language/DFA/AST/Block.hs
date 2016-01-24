module Language.DFA.AST.Block where

-- Basic Block of Stmt

import Language.DFA.AST.Def
import Language.DFA.Core.Label
import Data.Map hiding (map)

-- blocks inside a statement
class ToBlocks ast where
    blocks :: Label a => ast a -> Map a Block

instance ToBlocks Stmt where
    blocks s = case s of
        Assign l x a            -> singleton l (BAssign x a)
        Skip l                  -> singleton l BSkip
        Seq s1 s2               -> blocks s1 `union` blocks s2
        IfThenElse (bexp, l) s1 s2 ->
            insert l (BBExp bexp) (blocks s1 `union` blocks s2)
        While (bexp, l) s       -> insert l (BBExp bexp) (blocks s)

instance ToBlocks Program where
    blocks (Program procs s) = mconcat (map blocks procs) `union` blocks s

instance ToBlocks Proc where
    blocks (Proc _ _ _ is s end) =
        fromList [(is, BIs), (end, BEnd)] `union` blocks s