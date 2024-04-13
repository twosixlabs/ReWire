{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire
import ReWire.Bits

type Re i s o = ReacT i o (StateT s Identity) 

type W8 = W 8

csa :: (W8 , W8 , W8) -> (W8, W8)
csa (a , b , c) = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )

-- |
-- | Example 1. CSA
-- |
-- | The only thing this does is take its inputs i, computes csa on them, and
-- | output the results every clock cycle.

dev :: (W8, W8, W8) -> Re (W8, W8, W8) () (W8, W8) ()
dev (a, b, c) = signal (csa (a , b , c)) >>= dev

start :: ReacT (W8, W8, W8) (W8, W8) Identity ()
start = extrude (dev (lit 0, lit 0, lit 0)) ()

main = undefined
