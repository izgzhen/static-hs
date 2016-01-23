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


{-
    example 4:

    if [a > b]^1 {
        [x := b - 1]^2
        [y := a - b]^3
    } else {
        [y := b - a]^4
        [x := a - b]^5
    }
-}

example4 :: Stmt Label
example4 = IfThenElse (BInfixA (AVar "a") Greater (AVar "b"), Label 1)
                      (Assign (Label 2) "x" (AInfix (AVar "b") Minus (AVar "a")) `Seq`
                       Assign (Label 3) "y" (AInfix (AVar "a") Minus (AVar "b")))
                      (Assign (Label 4) "y" (AInfix (AVar "b") Minus (AVar "a")) `Seq`
                       Assign (Label 5) "x" (AInfix (AVar "a") Minus (AVar "b")))

{-
    example 5:

    [x := 2]^1
    [y := 4]^2
    [x := 1]^3
    if [y > x]^4 {
        [z := y]^5
    } else {
        [z := y * y]^6
    }
    [x := z]^7
-}

example5 :: Stmt Label
example5 = Assign (Label 1) "x" (ANum 2) `Seq`
           Assign (Label 2) "y" (ANum 4) `Seq`
           Assign (Label 3) "x" (ANum 1) `Seq`
           IfThenElse (BInfixA (AVar "y") Greater (AVar "x"), Label 4)
                            (Assign (Label 5) "z" (AVar "y"))
                            (Assign (Label 6) "z" (AInfix (AVar "y") Multiply (AVar "y"))) `Seq`
           Assign (Label 7) "x" (AVar "z")


