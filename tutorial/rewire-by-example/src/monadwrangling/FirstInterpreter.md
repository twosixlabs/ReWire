## Simple Arithmetic Expressions

The first interpreter, found in [Arith.hs](Arith.hs), defines a language `Exp` that has integer constants, negation, and addition. These correspond to the constructors `Const`, `Neg`, and `Add` of the `Exp` data type. The interpreter `eval0` does not use a monad and should be fairly self-explanatory.

```haskell
module Arith where

data Exp = Const Int | Neg Exp | Add Exp Exp

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2

eval0 :: Exp -> Int
eval0 (Const i)   = i
eval0 (Neg e)     = - (eval0 e)
eval0 (Add e1 e2) = eval0 e1 + eval0 e2

c = Const 99
n = Neg c
a = Add c n
```

Loading this into GHCi gives you what you'd expect:
```
λ> a
99 + -99
λ> eval0 a
0
```
