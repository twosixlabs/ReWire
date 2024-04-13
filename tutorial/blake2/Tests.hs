{-# LANGUAGE DataKinds #-}
module Tutorial.Blake2.Tests where

import Prelude hiding ((^), (+), (==), (&&))
import ReWire.Bits
import ReWire.Vectors
import ReWire
import ReWire.Interactive

import Tutorial.Blake2.Reference



newtype T8 a  = T8 (a , a , a , a , a , a , a , a )
-- newtype T16 a = T16 (a , a , a , a , a , a , a , a , a , a , a , a , a , a , a , a )

instance ShowHex a => Show (T8 a) where
  show (T8 (a0 , a1 , a2 , a3 , a4 , a5 , a6 , a7 )) = "(" Prelude.++ xshow a0 Prelude.++ " , " Prelude.++
                                                              xshow a1 Prelude.++ " , " Prelude.++
                                                              xshow a2 Prelude.++ " , " Prelude.++
                                                              xshow a3 Prelude.++ " , " Prelude.++
                                                              xshow a4 Prelude.++ " , " Prelude.++
                                                              xshow a5 Prelude.++ " , " Prelude.++
                                                              xshow a6 Prelude.++ " , " Prelude.++
                                                              xshow a7 Prelude.++ ")"



-- Test input : 

-- |
-- | matches output from RFC7693.
-- |

ex1 = T8 $ blake2b (lit 3) (lit 0) (lit 64) testMessage

-- blake2b_S (toW128 3) (toW64 0) (toW64 64) (toW64 0x636261, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0)
-- generates the example from RFC7693.

testMessage :: W64x16
testMessage = (lit 0x636261, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0)

i0 :: W64x16
i0 = (lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0,lit 0x0)

o0 :: W64x8
o0 = (lit 0x956987df86f6a32f,lit 0xc7c4745d2e7c7e16,lit 0x440efe68808fe4b6,lit 0x4c90f780d4448320,lit 0xebe35f11443e9636,lit 0xb4bc284c69c83a2a,lit 0x48792e6f27f3a0f5,lit 0x4b6e507a0519827d)

out1 = (0xf63ee0a11633a24c , 0xb5c350a475e7d625 , 0xb7140d7c03ace64f , 0x1e95b436cfd64508 , 0x93b9f66e28e54589 , 0xef896a41aa34a179 , 0xf3b7a9aa6106737c , 0xd61af9651b8ed55d)

out2 = (0xf63ee0a11633a24c,0xb5c350a475e7d625,0xb7140d7c03ace64f,0x1e95b436cfd64508,0x93b9f66e28e54589,0xef896a41aa34a179,0xf3b7a9aa6106737c,0xd61af9651b8ed55d)
