{-# LANGUAGE DataKinds #-}

import Prelude hiding ((^), (+))
import ReWire.Bits
import ReWire

-- | ReWire compiler will complain if this is imported
import ReWire.Interactive

type W8  = W 8

f :: W8 -> W8 -> W8 -> (W8, W8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )

-- |
-- | Example 1. CSA
-- |
-- | The only thing this does is take its inputs i, computes csa on them, and
-- | output the results every clock cycle.

csa :: (W8, W8, W8) -> ReacT (W8, W8, W8) (W8, W8) (StateT () Identity) ()
csa (a, b, c) = do
                   abc' <- signal (f a b c)
                   csa abc'

_40 , _25 , _20 , _41 , _0 :: W 8
_40 = lit 40
_25 = lit 25
_20 = lit 20
_41 = lit 41
_0  = lit 0

start :: ReacT (W8, W8, W8) (W8, W8) Identity () 
start = extrude (csa (_0, _0, _0)) ()

snapshot0 :: ((W8 , W8 , W8) , () , (W8 , W8))
snapshot0 = ( i0 , s0 , o0 )
   where
      i0 = (_0  , _0  , _0)
      s0 = ()
      o0 = (_0  , _0)
      
inputs :: [(W8 , W8 , W8)]
inputs = (_40 , _25 , _20)
       : (_41 , _25 , _20)
       : (_40 , _25 , _20)  : []

