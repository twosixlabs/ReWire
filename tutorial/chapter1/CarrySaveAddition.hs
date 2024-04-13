{-# LANGUAGE DataKinds #-}
module CarrySaveAddition where
import Prelude hiding ((^))
import ReWire.Bits
import ReWire

-- | RWC will barf in this is imported
import ReWire.Interactive 

type W8 = W 8

f :: W8 -> W8 -> W8 -> (W8, W8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. (lit 1) , (a ^ b) ^ c )
