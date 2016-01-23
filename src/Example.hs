module Example where

import AST
import Label

{-
    example:

    [z := 1]^1
    while [x > 0]^2 {
        [z = z * y]^3
        [x = x - 1]^4
    }
-}

example :: Stmt Label
example = Assign (Label 1) "z" (ANum 1) `Seq`
          While (BInfixA (AVar "x") Greater (ANum 0), Label 2)
                (Assign (Label 3) "z" (AInfix (AVar "z") Multiply (AVar "y")) `Seq`
                 Assign (Label 4) "x" (AInfix (AVar "x") Minus (ANum 1)))

{-
    example2:

    [x = a + b]^1
    [y = a * b]^2
    while [y > a + b]^3 {
        [a = a + 1]^4
        [x = a + b]^5
    }
-}

example2 :: Stmt Label
example2 = Assign (Label 1) "x" (AInfix (AVar "a") Plus (AVar "b")) `Seq`
           Assign (Label 2) "y" (AInfix (AVar "a") Multiply (AVar "b")) `Seq`
           While (BInfixA (AVar "y") Greater (AInfix (AVar "a") Plus (AVar "b")), Label 3)
                 (Assign (Label 4) "a" (AInfix (AVar "a") Plus (ANum 1)) `Seq`
                  Assign (Label 5) "x" (AInfix (AVar "a") Plus (AVar "b")))

{-
    example 3:

    [x := 5]^1
    [y := 1]^2
    while [x > 1]^3 {
        [y := x * y]^4
        [x := x - 1]^5
    }
-}

example3 :: Stmt Label
example3 = Assign (Label 1) "x" (ANum 5) `Seq`
           Assign (Label 2) "y" (ANum 1) `Seq`
           While (BInfixA (AVar "x") Greater (ANum 1), Label 3)
                 (Assign (Label 4) "y" (AInfix (AVar "x") Multiply (AVar "y")) `Seq`
                  Assign (Label 5) "x" (AInfix (AVar "x") Minus (ANum 1)))





