## The Obligatory Fibonacci Example


The following Haskell code (the file is called [Fib.hs](code/Fib.hs)) creates an infinite list of `Int`s in a conventional manner using the `fibgen` function.
```haskell
module Fibonacci where

fibs :: [Int]
fibs = fibgen 0 1
  where
    fibgen :: Int -> Int -> [Int]
    fibgen n m = n : fibgen m (n + m)
```

Loading `Fib.hs` into GHCi, you can see that it calculates the familiar Fibonacci sequence:
```haskell
ghci> take 10 fibs
take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
```

### Making Hardware Out of This.

In the ReWire code below, `fibdev` plays the same role as `fibgen` above. For the moment, just ignore the monadic type, `ReacT Bit (W 8) Identity ()`. (I'll explain its significance shortly.) Instead of using Haskell's `Int` type, we will compute over eight bit words (i.e., `W 8`). There is also a definition of `start`, which is a special symbol that unsurprisingly specifies how to start the device.

What `fibdev` does is, given two words `n` and `m`, it puts `n` on the output port using `signal` and accepts a new input `b` off of the input port. If bit `b` is `1`, then it continues on. However, if `b` is `0`, then it calls itself on `m` and `m + n` just like `fibgen` above.

```haskell
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

start :: ReacT Bit (W 8) Identity ()
start = fibdev (lit 0) (lit 1)

fibdev :: W 8 -> W 8 -> ReacT Bit (W 8) Identity ()
fibdev n m = do b <- signal n
                if b then fibdev n m else fibdev m (n + m)
```

### Lessons Learned.

There are some lessons to be learned from this example.
- Just like a state machine, every ReWire device has to have a `start`. 
- Most ReWire programs will begin with something like the top three lines of the previous ReWire code. 
  - There may be Haskell `Prelude` operations that have a particular meaning in ReWire (e.g., `+`), and so they may need to be hidden explicitly.
  - The other parts of that incantation is performed to use built-in words and their operations.
