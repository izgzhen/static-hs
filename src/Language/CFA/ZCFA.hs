-- 0-CFA analysis

module Language.CFA.ZCFA where

import Language.CFA.Fun

import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (union)
import Control.Monad.State
import Control.Monad.Reader

-- | Abstract value
type AbsVal   = S.Set Term
-- | Abstract Environment
type AbsEnv   = M.Map Name  AbsVal
-- | Abstract Cache
type AbsCache = M.Map Label AbsVal

data Constaint = Subset AbsVal AbsVal               -- lhs ⊆ rhs
               | IfSubset Term AbsVal AbsVal AbsVal -- {t} ⊆ rhs' => lhs ⊆ rhs
                 deriving (Show, Eq, Ord)

type Constaints = S.Set Constaint

type CFA = ReaderT [Term] (State (AbsCache, AbsEnv))

cacheOf :: Label -> CFA AbsVal
cacheOf l = lookup l <$> gets fst

cache :: CFA (Label -> AbsVal)
cache = do
    m <- gets fst
    return $ \l -> lookup l m

bindOf :: Name -> CFA AbsVal
bindOf l = lookup l <$> gets snd

bind :: CFA (Name -> AbsVal)
bind = do
    m <- gets snd
    return $ \l -> lookup l m

allTerms :: CFA [Term]
allTerms = ask

genCon :: Expr -> CFA Constaints
genCon (Expr l (TConst i)) = return $ S.empty
genCon (Expr l (TVar x))   = do
    rx <- bindOf x
    cl <- cacheOf l
    return $ S.singleton (Subset rx cl)
genCon (Expr l t@(TFn x e))  = do
    cl <- cacheOf l
    ce <- genCon e
    return $ S.singleton (Subset (S.singleton t) cl) `union` ce
genCon (Expr l t@(TFun f x e)) = do
    cl <- cacheOf l
    ce <- genCon e
    rf <- bindOf f
    return $ S.singleton (Subset (S.singleton t) cl) `union`
             S.singleton (Subset (S.singleton t) rf) `union` ce
genCon (Expr l (TApp e1@(Expr l1 t1) e2@(Expr l2 t2))) = do
    ce1 <- genCon e1
    ce2 <- genCon e2
    cl  <- cacheOf l
    cl1 <- cacheOf l1
    cl2 <- cacheOf l2
    ts  <- allTerms
    r   <- bind
    c   <- cache
    let case1 = S.fromList [ IfSubset t cl1 cl2 (r x)
                           | t@(TFn x (Expr l0 t0)) <- ts ]
    let case2 = S.fromList [ IfSubset t cl1 (c l0) cl
                           | t@(TFn x (Expr l0 t0)) <- ts ]
    let case3 = S.fromList [ IfSubset t cl1 cl2 (r x)
                           | t@(TFun _ x (Expr l0 t0)) <- ts ]
    let case4 = S.fromList [ IfSubset t cl1 (c l0) cl
                           | t@(TFun _ x (Expr l0 t0)) <- ts ]

    return $ ce1 `union` ce2 `union` case1 `union`
             case2 `union` case3 `union` case4

genCon (Expr l (TIf e0@(Expr l0 t0) e1@(Expr l1 t1) e2@(Expr l2 t2))) = do
    ce0 <- genCon e0
    ce1 <- genCon e1
    ce2 <- genCon e2
    cl  <- cacheOf l
    cl1 <- cacheOf l1
    cl2 <- cacheOf l2
    return $ ce0 `union` ce1 `union` ce2 `union`
             S.singleton (Subset cl1 cl) `union`
             S.singleton (Subset cl2 cl)

genCon (Expr l (TLet x e1@(Expr l1 t1) e2@(Expr l2 t2))) = do
    ce1 <- genCon e1
    ce2 <- genCon e2
    cl  <- cacheOf l
    rx  <- bindOf x
    cl1 <- cacheOf l1
    cl2 <- cacheOf l2
    return $ ce1 `union` ce2 `union`
             S.singleton (Subset cl1 rx) `union`
             S.singleton (Subset cl2 cl)

genCon (Expr _ (TOp e1 _ e2)) = union <$> genCon e1 <*> genCon e2

lookup :: (Show k, Ord k) => k -> M.Map k v -> v
lookup k m = case M.lookup k m of
    Just v  -> v
    Nothing -> error $ "can't find " ++ show k


