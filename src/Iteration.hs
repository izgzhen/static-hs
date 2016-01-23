-- Chaotic Iteration

module Iteration where

import qualified Data.Map as M
import Debug.Trace

chaotic :: (Show sol, Eq sol) => sol -> (sol -> sol) -> sol
chaotic initial improve = f initial (improve initial) 0
    where
        f a b i = traceLog i a $
            if a == b
                then a
                else f b (improve b) (i + 1)

        traceLog i a = trace $ "------ Iteration " ++ show i ++ " ------ \n" ++ show a

