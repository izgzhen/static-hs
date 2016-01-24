{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

-- Labelled Instance for Stmt

module Language.DFA.AST.Label where

import Language.DFA.AST.Def
import Language.DFA.Common
import Language.DFA.Core.Label
import Language.DFA.AST.Block
import Language.DFA.AST.Recursive
import Data.Set hiding (foldr, map)
import qualified Data.Map as M

labels :: (ToBlocks ast, Label a) => ast a -> Set a
labels ast = fromList $ M.keys (blocks ast)

instance Label a => Labelled Stmt a where
    -- init function
    initLabel (Assign l _ _) = l
    initLabel (Skip l) = l
    initLabel (Seq s1 s2) = initLabel s1
    initLabel (IfThenElse (_, l) _ _) = l
    initLabel (While (bexp, l) _) = l

    -- final function
    finalLabels (Assign l _ _) = singleton l
    finalLabels (Skip l) = singleton l
    finalLabels (Seq s1 s2) = finalLabels s2
    finalLabels (IfThenElse _ s1 s2) = finalLabels s1 `union` finalLabels s2
    finalLabels (While (bexp, l) _) = singleton l

    -- flow function
    flow (Assign _ _ _) = empty
    flow (Skip _) = empty
    flow (Seq s1 s2) = flow s1 `union` flow s2 `union`
                        fromList [ Intrap (l, initLabel s2) | l <- toList $ finalLabels s1]
    flow (IfThenElse (bexp, l) s1 s2) =
        flow s1 `union` flow s2 `union` fromList [ Intrap (l, initLabel s1), Intrap (l, initLabel s2)]
    flow (While (bexp, l) s) =
        flow s `union` fromList (Intrap (l, initLabel s) : [ Intrap (l', l) | l' <- toList $ finalLabels s])


instance Label a => Labelled Program a where
    initLabel (Program _ s)   = initLabel s
    finalLabels (Program _ s) = finalLabels s
    flow (Program procs s)    = mconcat (map flow procs) `union` flow s

instance Label a => Labelled Proc a where
    initLabel (Proc _ _ _ is _ _)    = is
    finalLabels (Proc _ _ _ _ _ end) = singleton end
    flow (Proc _ _ _ is s end)       =
        singleton (Interp (is, initLabel s)) `union` flow s `union`
            fromList (map Interp (zip (toList $ finalLabels s) (repeat end)))


instance Label a => InterLabelled Program a where
    interflow (Program procs s) =
        fromList [ (lc, ln, lx, lr)
                 | Call fcs _ _ lc lr <- collectCalls s
                 , Proc fce _ _ ln _ lx <- procs
                 , fcs == fce ]

collectCalls :: Stmt a -> [Stmt a]
collectCalls = collect f
        where
            f s@(Call _ _ _ _ _) = single s
            f _ = mempty



