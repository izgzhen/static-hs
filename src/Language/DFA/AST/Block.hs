module Language.DFA.AST.Block where

-- Basic Block of Stmt

import Language.DFA.AST.Def
import Language.DFA.Core.Label
import Data.Map hiding (map)

-- Blocks inside a statement
class ToBlocks ast where
    toBlocks :: Label a => ast a -> Map a Block

instance ToBlocks Stmt where
    toBlocks s = case s of
        Assign l x a            -> singleton l (BAssign x a)
        Skip l                  -> singleton l BSkip
        Seq s1 s2               -> toBlocks s1 `union` toBlocks s2
        IfThenElse (bexp, l) s1 s2 ->
            insert l (BBExp bexp) (toBlocks s1 `union` toBlocks s2)
        While (bexp, l) s       -> insert l (BBExp bexp) (toBlocks s)
        Call x ins outs is end  -> fromList [ (is, BCall x ins outs)
                                            , (end, BCall x ins outs) ]

instance ToBlocks Program where
    toBlocks (Program procs s) = mconcat (map toBlocks procs) `union` toBlocks s

instance ToBlocks Proc where
    toBlocks (Proc _ _ _ is s end) =
        fromList [(is, BIs), (end, BEnd)] `union` toBlocks s