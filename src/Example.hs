module Example where

import AST
import Label

example :: Stmt Label
example = Assign (Label 1) "z" (ANum 1) `Seq`
          While (BInfixA (AVar "x") Greater (ANum 0), Label 2)
                (Assign (Label 3) "z" (AInfix (AVar "z") Multiply (AVar "y")) `Seq`
                 Assign (Label 4) "x" (AInfix (AVar "x") Minus (ANum 1)))
