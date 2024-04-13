{-# LANGUAGE DataKinds #-}
module CSAdd where

import Prelude hiding ((^))
import ReWire()
import ReWire.Bits

type W8 = W 8

csa :: (W8 , W8 , W8) -> (W8, W8)
csa (a , b , c) = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )
