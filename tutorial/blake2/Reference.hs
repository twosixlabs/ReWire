{-# LANGUAGE DataKinds #-}
module Tutorial.Blake2.Reference where

-- | This is an update of Blake2bReference to rely entirely on rewire-user.

import Prelude hiding ((^), (+), (==), (&&))
import ReWire.Bits
import ReWire.Vectors
import ReWire
import ReWire.Interactive

-----------------------------
-- Definitions and helpers
-----------------------------

type W8   = W 8
type W64  = W 64
type W128 = W 128

-- NOTE: Here we have the 10 permutations hardcoded, which is how it is currently 
-- being implemented in the ReWire version. Below we've commented out the original
-- implementation.

type W64x8  = (W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 )
type W64x16 = (W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 )

perm0 , perm1 , perm2 , perm3 , perm4 , perm5 , perm6 , perm7 , perm8 , perm9 :: W64x16 -> W64x16

perm0 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
perm1 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w14,w10,w4,w8,w9,w15,w13,w6,w1,w12,w0,w2,w11,w7,w5,w3)
perm2 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w11,w8,w12,w0,w5,w2,w15,w13,w10,w14,w3,w6,w7,w1,w9,w4)
perm3 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w7,w9,w3,w1,w13,w12,w11,w14,w2,w6,w5,w10,w4,w0,w15,w8)
perm4 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w9,w0,w5,w7,w2,w4,w10,w15,w14,w1,w11,w12,w6,w8,w3,w13)
perm5 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w2,w12,w6,w10,w0,w11,w8,w3,w4,w13,w7,w5,w15,w14,w1,w9)
perm6 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w12,w5,w1,w15,w14,w13,w4,w10,w0,w7,w6,w3,w9,w2,w8,w11)
perm7 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w13,w11,w7,w14,w12,w1,w3,w9,w5,w0,w15,w4,w8,w6,w2,w10)
perm8 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w6,w15,w14,w9,w11,w3,w0,w8,w12,w2,w13,w7,w1,w4,w10,w5)
perm9 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w10,w2,w8,w4,w7,w6,w1,w5,w15,w11,w9,w14,w3,w12,w13,w0)

doubleXor :: W n -> W n -> W n -> W n
doubleXor x y z  = x ^ y ^ z

split16 :: W64x16 -> (W64x8,W64x8)
split16 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = ((a0,a1,a2,a3,a4,a5,a6,a7),(b0,b1,b2,b3,b4,b5,b6,b7))

map3x8 :: (W64 -> W64 -> W64 -> W64) -> W64x8 -> W64x8 -> W64x8 -> W64x8
map3x8 f (a0,a1,a2,a3,a4,a5,a6,a7) (b0,b1,b2,b3,b4,b5,b6,b7) (c0,c1,c2,c3,c4,c5,c6,c7)
   = (f a0 b0 c0, f a1 b1 c1, f a2 b2 c2, f a3 b3 c3, f a4 b4 c4, f a5 b5 c5, f a6 b6 c6, f a7 b7 c7)

-- | just for sanity checking helper functions

c :: W 128
c = lit 0xbaddd00dbaddd00ddeadbeefdeadbeef

x :: W64
x = lit 0xd0d0cacadeadbeef

w :: W64
w = lit 0x0a1b2c3d4e5f6789
y :: W 32
y = slice (Proxy :: Proxy 32) x

z :: W 32
z = rslice (Proxy :: Proxy 32) x

-- | end of sanity check

lowhalf :: W128 -> W64
lowhalf w = slice (Proxy :: Proxy 64) w

highhalf :: W128 -> W64
highhalf w = rslice (Proxy :: Proxy 64) w

shiftl8 :: W64 -> W64
shiftl8 w = w <<. lit 8

-- | some sanity checks
-- ghci> hex c
--    0xbaddd00dbaddd00ddeadbeefdeadbeef
-- ghci> hex $ lowhalf c
--    0xdeadbeefdeadbeef
-- ghci> hex $ hihalf c
--    0xbaddd00dbaddd00d
-- ghci> hex $ shiftl8 x
--   0xd0cacadeadbeef00

iv0 , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7 :: W64
iv0 = lit 0x6a09e667f3bcc908
iv1 = lit 0xbb67ae8584caa73b
iv2 = lit 0x3c6ef372fe94f82b
iv3 = lit 0xa54ff53a5f1d36f1
iv4 = lit 0x510e527fade682d1
iv5 = lit 0x9b05688c2b3e6c1f
iv6 = lit 0x1f83d9abfb41bd6b
iv7 = lit 0x5be0cd19137e2179

iv :: W64x8
iv = (iv0,iv1,iv2,iv3,iv4,iv5,iv6,iv7)

app :: W64x8 -> W64x8 -> W64x16
app (a0,a1,a2,a3,a4,a5,a6,a7) (b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7)

update1213 :: W64 -> W64 -> W64x16 -> W64x16
update1213 n12 n13 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,n12,n13,b6,b7)

update14 :: W64 -> W64x16 -> W64x16
update14 n14 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,n14,b7)

update0 :: W64 -> W64x8 -> W64x8
update0 n (a0,a1,a2,a3,a4,a5,a6,a7) = (n,a1,a2,a3,a4,a5,a6,a7)

w64toW8x8 :: W64 -> (W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 )
w64toW8x8 w64 = (s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7)
  where
    s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7 :: W8
    s0 = slice (Proxy :: Proxy 0)  w64
    s1 = slice (Proxy :: Proxy 8)  w64
    s2 = slice (Proxy :: Proxy 16) w64
    s3 = slice (Proxy :: Proxy 24) w64
    s4 = slice (Proxy :: Proxy 32) w64
    s5 = slice (Proxy :: Proxy 40) w64
    s6 = slice (Proxy :: Proxy 48) w64
    s7 = slice (Proxy :: Proxy 56) w64

-- | just for sanity check
gitrdone (s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7) =
      (xshow s0 , xshow s1 , xshow s2 , xshow s3 , xshow s4 , xshow s5 , xshow s6 , xshow s7)

-- ghci> gitrdone $ w64toW8x8 x
--   ("0xd0","0xd0","0xca","0xca","0xde","0xad","0xbe","0xef")
-- ghci> gitrdone $ w64toW8x8 w
--   ("0x0a","0x1b","0x2c","0x3d","0x4e","0x5f","0x67","0x89")

type W8x64  = ( W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8
              , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 
              , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 
              , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 )

rev64bytes :: W8x64 -> W8x64
rev64bytes ( w0  ,  w1 ,  w2 ,  w3 ,  w4 ,  w5 ,  w6 ,  w7 ,  w8 ,  w9 , w10 , w11 , w12 , w13 , w14 , w15
           , w16 , w17 , w18 , w19 , w20 , w21 , w22 , w23 , w24 , w25 , w26 , w27 , w28 , w29 , w30 , w31
           , w32 , w33 , w34 , w35 , w36 , w37 , w38 , w39 , w40 , w41 , w42 , w43 , w44 , w45 , w46 , w47
           , w48 , w49 , w50 , w51 , w52 , w53 , w54 , w55 , w56 , w57 , w58 , w59 , w60 , w61 , w62 , w63)
   = ( w63 , w62 , w61 , w60 , w59 , w58 , w57 , w56 , w55 , w54 , w53 , w52 , w51 , w50 , w49 , w48
     , w47 , w46 , w45 , w44 , w43 , w42 , w41 , w40 , w39 , w38 , w37 , w36 , w35 , w34 , w33 , w32
     , w31 , w30 , w29 , w28 , w27 , w26 , w25 , w24 , w23 , w22 , w21 , w20 , w19 , w18 , w17 , w16
     , w15 , w14 , w13 , w12 , w11 , w10 , w9  ,  w8 ,  w7 ,  w6 ,  w5 ,  w4 ,  w3 ,  w2 ,  w1 ,  w0)

------------------------------------------------
-- BLAKE2b, composition-representation version
------------------------------------------------

--Mixing function G
mixG :: ( W64 , W64 , W64 , W64 ) -> W64 -> W64 -> ( W64 , W64 , W64 , W64 )
mixG (vsa,vsb,vsc,vsd) x y = (vsa2,vsb2,vsc2,vsd2)
    where
      vsa1 = vsa + vsb + x 
      vsd1 = rotR (lit 32) (vsd ^ vsa1)
      vsc1 = vsc + vsd1
      vsb1 = rotR (lit 24) (vsb ^ vsc1)
      
      vsa2 = vsa1 + vsb1 + y
      vsd2 = rotR (lit 16) (vsd1 ^ vsa2)
      vsc2 = vsc1 + vsd2
      vsb2 = rotR (lit 63) (vsb1 ^ vsc2)

type W64x4x4 = ((W64, W64, W64, W64), (W64, W64, W64, W64), (W64, W64, W64, W64), (W64, W64, W64, W64))

mixGRound :: W64x8 -> W64x4x4 -> W64x4x4
mixGRound (m0,m1,m2,m3, m4,m5,m6,m7) (vs0,vs1,vs2,vs3)
    = (g0,g1,g2,g3)
    where
      g0 = mixG vs0 m0 m1
      g1 = mixG vs1 m2 m3
      g2 = mixG vs2 m4 m5
      g3 = mixG vs3 m6 m7

pipeGRound0 :: W64x16 -> W64x4x4
pipeGRound0 (v0,v1,v2,v3, v4,v5,v6,v7, v8,v9,v10,v11, v12,v13,v14,v15)
   = ((v0,v4,v8, v12)
     ,(v1,v5,v9, v13)
     ,(v2,v6,v10,v14)
     ,(v3,v7,v11,v15))

unpipeGRound0 :: W64x4x4 -> W64x16
unpipeGRound0 ((v0,v4,v8, v12)
              ,(v1,v5,v9, v13)
              ,(v2,v6,v10,v14)
              ,(v3,v7,v11,v15))
   = (v0,v1,v2,v3, v4,v5,v6,v7, v8,v9,v10,v11, v12,v13,v14,v15)

pipeGRound1 :: W64x16 -> W64x4x4
pipeGRound1 (v0,v1,v2,v3, v4,v5,v6,v7, v8,v9,v10,v11, v12,v13,v14,v15)
   = ((v0,v5,v10,v15)
     ,(v1,v6,v11,v12)
     ,(v2,v7,v8, v13)
     ,(v3,v4,v9, v14))

unpipeGRound1 :: W64x4x4 -> W64x16
unpipeGRound1 ((v0,v5,v10,v15)
              ,(v1,v6,v11,v12)
              ,(v2,v7,v8, v13)
              ,(v3,v4,v9, v14))
   = (v0,v1,v2,v3, v4,v5,v6,v7, v8,v9,v10,v11, v12,v13,v14,v15)

--Cryptographic mixing for compression function F
moreMix :: W64x16 -> W64x16 -> W64x16
moreMix ms = 
      unpipeGRound1 . mixGRound ms1 . pipeGRound1 
    . unpipeGRound0 . mixGRound ms0 . pipeGRound0
    where
      (ms0,ms1) = split16 ms

type W64x16x10 = (W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16)

prepermute :: W64x16 -> W64x16x10
prepermute ms = (perm0 ms, perm1 ms, perm2 ms, perm3 ms,
                 perm4 ms, perm5 ms, perm6 ms, perm7 ms,
                 perm8 ms, perm9 ms)

mixLoop :: W64x16x10 -> W64x16 -> W64x16
mixLoop ms = moreMix ms1 . 
             moreMix ms0 . 
             moreMix ms9 . 
             moreMix ms8 . 
             moreMix ms7 . 
             moreMix ms6 . 
             moreMix ms5 . 
             moreMix ms4 . 
             moreMix ms3 . 
             moreMix ms2 . 
             moreMix ms1 . 
             moreMix ms0
  where
    (ms0,ms1,ms2,ms3,ms4,ms5,ms6,ms7,ms8,ms9) = ms

finalStep :: (W64x16,W64x8) -> W64x8
finalStep (vs,hs) = map3x8 doubleXor hs vs1 vs2
  where
    (vs1,vs2) = split16 vs

initHs :: W64 -> W64 -> W64x8
initHs kk nn = update0 (iv0 ^ lit 0x01010000 ^ shiftl8 kk ^ nn) iv

initVs :: (W128,Bit,W64x8) -> (W64x16, W64x8)
initVs (t,f,hs) = (update1213 (iv4 ^ lowhalf t) (iv5 ^ highhalf t)
                              (if f 
                               then update14 (bnot iv6) (app hs iv) 
                               else app hs iv)
                  , hs)

(<#>) :: (a -> b) -> (c -> d) -> (a , c) -> (b , d)
f <#> g = \ (a , c) -> (f a, g c)



--Compression function F
compressF :: (W64x16,(W128,Bit,W64x8)) -> W64x8
compressF = 
     finalStep 
     . (uncurry mixLoop <#> id)
     . (\ (ms,(vs,hs)) -> ((ms,vs),hs)) 
     . (prepermute <#> initVs)

setOffset :: W64 -> W128 -> W128
setOffset kk ll = if kk == lit 0 then ll else ll + lit 128

--------------------------------------------------------------
-- single-iteration version of blake2b for easy rewire-izing
--------------------------------------------------------------

-- Parameters:
ll :: W128
ll = lit 0
kk :: W64
kk = lit 64
nn :: W64
nn = lit 64

blake2b :: W128 -> W64 -> W64 -> W64x16 -> W64x8
blake2b ll kk nn = flip (curry compressF) (setOffset kk ll, one, initHs kk nn)
   -- | ^^^^^ There are (evidently unstated) assumptions about what these parameters should be
   -- |       in the BLAKE2 testing harnesses. It'd be best of those are made explicit.

  

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

