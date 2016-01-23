-- The Monotone Framework

module Mono where

import qualified Data.Set  as S
import qualified Data.Map  as M

{-
    Property space: L
        + Complete lattice
        + Satisfies Ascending Chain Condition
        + Having a least upper bound operator `U`

    Transfer functions: F
        + L -> L
        + Contains identity function
        + Closed under function composition

    Distributivity:
        + f (l1 U l2) = f(l1) U f(l2)

    Flow:
        + flow(S)
        + reversed flow(S)

    Extermal labels:
        + init(S)
        + final(S)

    Extremal value:
        + Some l in L for extermal labels

    Mapping f:
        + Label -> F
-}


-- summarizeSingleStep src l sol =
--     foldr1 union [ reflect l' | (l', l) <- flow src ] `union` extermal
--         where
--             extermal = if l `member` extermalLabels src
--                             then extermalProperty
--                             else lBot

-- reflectSingleStep src l sol = updateSol l (transfer l src sol) sol

data Lattice l = Lattice {
  -- ⊑
  lessThen :: l -> l -> Bool
  -- ⊔
, meet :: l -> l -> l
  -- ⊥
, bottom :: l
}

data Analysis ast l a = Analysis {
  _lattice       :: Lattice l
, _extermals     :: ast a -> S.Set a
, _extermalVal   :: ast a -> l
, _flow          :: ast a -> S.Set (a, a)
, _transfer      :: ast a -> a -> Solution a l -> Solution a l
}

data Solution a l = Solution {
  _entry :: M.Map a l
, _exit  :: M.Map a l
}




