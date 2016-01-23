-- Chaotic Iteration

module Iteration where

import qualified Data.Map as M
import Debug.Trace

import Common

chaotic :: (Show sol, Eq sol) => DebugOption -> sol -> (sol -> sol) -> sol
chaotic opt initial improve = f initial (improve initial) 0
    where
        f a b i =
            let next = if a == b
                        then a
                        else f b (improve b) (i + 1)
            in  case opt of
                    ShowTrace -> traceLog i a next
                    NoTrace   -> next


        traceLog i a = trace $ "------ Iteration " ++ show i ++ " ------ \n" ++ show a

