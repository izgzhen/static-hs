module Label where

import AST
import Data.Set hiding (foldr)

newtype Label = Label { unLabel :: Int } deriving (Eq, Ord)

instance Show Label where
    show (Label i) = show i

type Edge = (Label, Label)

class Labelled a where
    initLabel :: a Label -> Label
    finalLabels :: a Label -> Set Label
    labels :: a Label -> Set Label
    flow :: a Label -> Set Edge

instance Labelled Stmt where
    initLabel (Assign l _ _) = l
    initLabel (Skip l) = l
    initLabel (Seq s1 s2) = initLabel s1
    initLabel (IfThenElse (_, l) _ _) = l
    initLabel (While (bexp, l) _) = l

    finalLabels (Assign l _ _) = singleton l
    finalLabels (Skip l) = singleton l
    finalLabels (Seq s1 s2) = finalLabels s2
    finalLabels (IfThenElse _ s1 s2) = finalLabels s1 `union` finalLabels s2
    finalLabels (While (bexp, l) _) = singleton l

    labels stmt = fromList $ foldr f [] (blocks stmt)
        where
            f (BBExp (_, l)) lbls = l : lbls
            f (BStmt (Assign l _ _)) lbls = l : lbls
            f (BStmt (Skip l)) lbls = l : lbls

    flow (Assign _ _ _) = empty
    flow (Skip _) = empty
    flow (Seq s1 s2) = flow s1 `union` flow s2 `union`
                        fromList [ (l, initLabel s2) | l <- toList $ finalLabels s1]
    flow (IfThenElse (bexp, l) s1 s2) =
        flow s1 `union` flow s2 `union` fromList [(l, initLabel s1), (l, initLabel s2)]
    flow (While (bexp, l) s) =
        flow s `union` fromList ((l, initLabel s) : [ (l', l) | l' <- toList $ finalLabels s])

data Block a = BBExp (BExp, a) | BStmt (Stmt a) deriving (Show, Eq, Ord)

type Blocks a = Set (Block a)

blocks :: Ord a => Stmt a -> Blocks a
blocks s = case s of
    Assign _ _ _    -> singleton $ BStmt s
    Skip _          -> singleton $ BStmt s
    Seq s1 s2       -> blocks s1 `union` blocks s2
    IfThenElse bexp s1 s2 ->
        singleton (BBExp bexp) `union` blocks s1 `union` blocks s2
    While bexp s -> singleton (BBExp bexp) `union` blocks s

