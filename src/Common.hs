module Common where

import qualified Data.Map as M
import Data.Maybe (fromJust)

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m
