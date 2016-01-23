{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- Labelled Instance for Stmt

module Language.DFA.AST.Label where

import Language.DFA.AST.Stmt
import Language.DFA.Core.Label
import Language.DFA.AST.Block
import Data.Set hiding (foldr)

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

    -- labels inside a statement
    labels stmt = fromList $ foldr f [] (blocks stmt)
        where
            f (BBExp (_, l)) lbls = l : lbls
            f (BStmt (Assign l _ _)) lbls = l : lbls
            f (BStmt (Skip l)) lbls = l : lbls

    -- flow function
    flow (Assign _ _ _) = empty
    flow (Skip _) = empty
    flow (Seq s1 s2) = flow s1 `union` flow s2 `union`
                        fromList [ (l, initLabel s2) | l <- toList $ finalLabels s1]
    flow (IfThenElse (bexp, l) s1 s2) =
        flow s1 `union` flow s2 `union` fromList [(l, initLabel s1), (l, initLabel s2)]
    flow (While (bexp, l) s) =
        flow s `union` fromList ((l, initLabel s) : [ (l', l) | l' <- toList $ finalLabels s])
