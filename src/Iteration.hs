-- Chaotic Iteration

module Iteration where

import qualified Data.Map as M

chaotic :: Eq sol => sol -> (sol -> sol) -> sol
chaotic initial improve = f initial (improve initial)
    where
        f a b = if a == b then b
                    else f b (improve b)


