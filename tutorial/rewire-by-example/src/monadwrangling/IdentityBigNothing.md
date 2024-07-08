# The `Identity` Monad is a Big Nothingburger

We introduce now the `Identity` monad, which doesn't really give you anything at all. I introduce it first because it uses Haskell's built-in monad syntax, and it's useful to meet that syntax first when the monad is just a big nothing. The code for this section is found in [IdentityMonad.hs](IdentityMonad.hs) and [IdentityMonadDo.hs](IdentityMonadDo.hs).

First, here's the new interpreter `eval1`. The salient point is that `eval0` and `eval1` are doing the same thing, but what's all this `return` and `>>=` business? (They're explained below if you want to skip ahead.)
```haskell
module IdentityMonad where

import Control.Monad.Identity -- this is new.

data Exp = Const Int | Neg Exp | Add Exp Exp

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2

eval1 :: Exp -> Identity Int
eval1 (Const i)   = return i
eval1 (Neg e)     = eval1 e >>= \ v -> return (- v)
eval1 (Add e1 e2) = eval1 e1 >>= \ v1 -> eval1 e2 >>= \ v2 -> return (v1 + v2)

c = Const 99
n = Neg c
a = Add c n
```

The `Identity` monad has the following definition (it's actually a simplification). 
```haskell
data Identity a = Identity a -- apologies for overloading the constructors.
return :: a -> Identity a
return v = Identity v
(>>=) :: Identity a -> (a -> Identity b) -> Identity b
(Identity v) >>= f = f v
```
So, `return` just injects its argument into `Identity`. The operation `>>=` (a.k.a., "bind") boils down to a backwards apply. It's just a whole lot of applying and pattern-matching on the `Identity` constructor, signifying nothing. When you load all this into GHCi, you get just what you'd expect:
```
λ> a
99 + -99
λ> eval1 a
Identity 0
λ> 
```

## Lessons Learned

As people say, `eval1` and `eval0` are morally equivalent, in the sense that, if you were so inclined, you could *prove* the equality `eval1 a = Identity (eval0 a)` holds for any `a`.

### Monadic Syntactic Sugar or Saccharine? 

Haskell overloads its monad syntax, so when we see the `>>=` and `return` again, they will be typed in different monads than `Identity`. Overloading is great for some uses, because it removes clutter. I find for formal methods it can be kind of confusing. So, reader beware!

There is also another shorthand for `>>=` that is frequently used called `do` notation; it's defined as:
```haskell
   x >>= f = do
               v <- x
		       f v
```
So, the clause of `eval1` for `Neg` is as follows when written in `do` notation:
```haskell
eval1 (Neg e)     = do
                      v <- eval1 e
                      return (- v)
```

The code `IdentityMonadDo.hs` just reformulates the code in `IdentityMonad.hs` using `do` notation.

