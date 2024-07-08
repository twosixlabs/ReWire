# 4th: Errors + Register

The code for this is [RegisterError.hs](RegisterError.hs). In this example, we want to add both a possibly error-producing computation along with the register. This is done mostly through monadic means.


```haskell
module Register where

import Control.Monad.State

data Exp = Const Int | Neg Exp | Add Exp Exp
         | Div Exp Exp  -- Both errors
         | X            -- and a register X

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2
  show X           = "X"
```

Here's how we handle this:
- Create a new monad from `Maybe` with an `Int` register: `StateT Int Maybe`
- This new monad has two operations
  - `get` that reads the current value of the register
  - `put` that updates the value of the register
- `StateT Int` is known as a *monad transformer*
  
The code below does just that  
```haskell
readX :: StateT Int Maybe Int
readX = get

eval3 :: Exp -> StateT Int Maybe Int
eval3 (Const i)   = return i
eval3 (Neg e)     = do
                      v <- eval3 e
                      return (- v)
eval3 (Add e1 e2) = do
                      v1 <- eval3 e1
                      v2 <- eval3 e2
                      return (v1 + v2)
eval3 (Div e1 e2) = do
                      v1 <- eval3 e1
                      v2 <- eval3 e2
                      if v2==0 then lift Nothing else return (v1 `div` v2)
                                 -- N.b., this is new.

eval3 X           = readX
```

