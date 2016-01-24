-- Inter-procedural analysis framework

module Language.DFA.Packages.Interp where

import Language.DFA.AST
import Language.DFA.Core.Mono

{-
    Context management:
    + Store
    + Binding

    Flow 2.0:
    + l_c, l_n, l_x, l_r
-}

interp :: Analysis Program Block l a


