{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits          -- importing operations on words
import Prelude hiding ((+)) -- hiding the standard Haskell (+)

-- | This version extends SimpleMealy to count Zero inputs.

{-
Current State | Input | Next State | Output 
--------------------------------------------
   si             0        s0          0
   si             1        s1          0
   s0             0        s0          0
   s0             1        s1          1
   s1             0        s0          1
   s1             1        s1          0
-}

data Alphabet = Zero | One

-- type Counter = Int 
type Counter = W 8

-- Technical note: ReWire doesn't have Int, so instead of using
--                 the first Counter definition, use the
--                 ReWire.Bits version.

inc :: ReacT Alphabet Alphabet (StateT Counter Identity) ()
inc = lift (get >>= \ i -> put (i + lit 1))

si , s0 , s1 :: Alphabet -> ReacT Alphabet Alphabet (StateT Counter Identity) ()

si Zero = inc >> signal Zero >>= s0
si One  = signal Zero >>= s1

s0 Zero = inc >> signal Zero >>= s0
s0 One  = signal One  >>= s1

s1 Zero = inc >> signal One  >>= s0
s1 One  = signal Zero >>= s1

start :: ReacT Alphabet Alphabet Identity ()
start = extrude (signal Zero >>= s0) (lit 0)

