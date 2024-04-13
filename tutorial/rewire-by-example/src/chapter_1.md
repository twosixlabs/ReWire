# Chapter 1


```haskell
module Fibonacci where

fibs :: [Int]
fibs = fibgen 0 1
  where
    fibgen :: Int -> Int -> [Int]
    fibgen n m = n : fibgen m (n + m)
```


```haskell
ghci> take 10 fibs
take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
```

```haskell
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

import ReWire.Interactive

start :: ReacT Bit (W 8) Identity ()
start = fib (lit 0) (lit 1)

fib :: W 8 -> W 8 -> ReacT Bit (W 8) Identity ()
fib n m = do b <- signal n
             if b then fib n m else fib m (n + m)
```
