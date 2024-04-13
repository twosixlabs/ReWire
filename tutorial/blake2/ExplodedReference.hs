{-# LANGUAGE DataKinds #-}
module Tutorial.Blake2.ExplodedReference where

-- | This recasts Reference in a less bureaucratic light.
-- | This is the version I want to use for the paper.

import Prelude hiding ((^), (+), (==), (&&))
import ReWire.Bits
import ReWire.Vectors
import ReWire
import ReWire.Interactive

import GHC.TypeNats

-----------------------------
-- Definitions and helpers
-----------------------------

type W8   = W 8
type W64  = W 64
type W128 = W 128
          
type W64x8  = (W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 )
type W64x16 = (W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 , W64 )
type W64x4x4 = ((W64, W64, W64, W64), (W64, W64, W64, W64), (W64, W64, W64, W64), (W64, W64, W64, W64))

(<<>>) :: (a -> b) -> (b -> c) -> a -> c
f <<>> g = g . f

(<#>) :: (a -> b) -> (c -> d) -> (a , c) -> (b , d)
f <#> g = \ (a , c) -> (f a, g c)

--
-- Refactoring the Reference
-- 

-- | I'm including code from that reference spec from Saarinen/Aumasson


-----------------------------
-- | Cyclic right rotation.
-----------------------------

-- // Cyclic right rotation.
-- #ifndef ROTR64
-- #define ROTR64(x, y)  (((x) >> (y)) ^ ((x) << (64 - (y))))
-- #endif

-----------------------------
-- | Little-endian byte access.
-----------------------------

-- // Little-endian byte access.
-- #define B2B_GET64(p)                               \
--   (((uint64_t) ((uint8_t *) (p))[0]) ^             \
--    (((uint64_t) ((uint8_t *) (p))[1]) << 8) ^      \
--    (((uint64_t) ((uint8_t *) (p))[2]) << 16) ^     \
--    (((uint64_t) ((uint8_t *) (p))[3]) << 24) ^     \
--    (((uint64_t) ((uint8_t *) (p))[4]) << 32) ^     \
--    (((uint64_t) ((uint8_t *) (p))[5]) << 40) ^     \
--    (((uint64_t) ((uint8_t *) (p))[6]) << 48) ^     \
--    (((uint64_t) ((uint8_t *) (p))[7]) << 56))


-----------------------------
-- |  3.1. Mixing function G.
-----------------------------

-- // G Mixing function.
-- #define B2B_G(a, b, c, d, x, y) {      \
--     v[a] = v[a] + v[b] + x;            \
--     v[d] = ROTR64(v[d] ^ v[a], 32);    \
--     v[c] = v[c] + v[d];                \
--     v[b] = ROTR64(v[b] ^ v[c], 24);    \
--     v[a] = v[a] + v[b] + y;            \
--     v[d] = ROTR64(v[d] ^ v[a], 16);    \
--     v[c] = v[c] + v[d];                \
--     v[b] = ROTR64(v[b] ^ v[c], 63); }

-- |
-- | 2.1. G Rotation constants
-- |
_R1 , _R2 , _R3 , _R4 :: W64
_R1 = lit 32
_R2 = lit 24
_R3 = lit 16
_R4 = lit 63

--Mixing function G
_G :: W64 -> W64 -> ( W64 , W64 , W64 , W64 ) -> ( W64 , W64 , W64 , W64 )
_G x y (va,vb,vc,vd) = (va2,vb2,vc2,vd2)
    where
      va1 = va + vb + x 
      vd1 = rotR _R1 (vd ^ va1)
      vc1 = vc + vd1
      vb1 = rotR _R2 (vb ^ vc1)
      
      va2 = va1 + vb1 + y
      vd2 = rotR _R3 (vd1 ^ va2)
      vc2 = vc1 + vd2
      vb2 = rotR _R4 (vb1 ^ vc2)

-- -- |
-- -- | Replaces mixG; called B2B_G in blake2b.c.
-- -- |
-- xmixG :: W64 -> W64 -> ( W64 , W64 , W64 , W64 ) -> ( W64 , W64 , W64 , W64 )
-- xmixG x y = gmix (lit 32) (lit 24) x <<>> gmix (lit 16) (lit 63) y
--    where
--      gmix :: GHC.TypeNats.KnownNat n => W n -> W n -> W n -> (W n, W n, W n, W n) -> (W n, W n, W n, W n)
--      gmix k1 k2 x (a,b,c,d) = let
--                                  a' = a + b + x              
--                                  d' = rotR k1 (d ^ a')
--                                  c' = c + d'                 
--                                  b' = rotR k2 (b ^ c')
--                               in
--                                  (a' , b' , c' , d')

-----------------------------
-- | 
-----------------------------


-----------------------------
-- | Initialization Vector
-----------------------------


-- // Initialization Vector.
-- static const uint64_t blake2b_iv[8] = {
--   0x6a09e667f3bcc908, 0xbb67ae8584caa73b,
--   0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
--   0x510e527fade682d1, 0x9b05688c2b3e6c1f,
--   0x1f83d9abfb41bd6b, 0x5be0cd19137e2179
-- };


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


-----------------------
-----------------------

mixGRound :: W64x8 -> W64x4x4 -> W64x4x4
mixGRound (m0,m1,m2,m3, m4,m5,m6,m7) (v0,v1,v2,v3)
    = (g0,g1,g2,g3)
    where
      g0 = _G m0 m1 v0 -- | these are just four independent calls
      g1 = _G m2 m3 v1 -- | to _G
      g2 = _G m4 m5 v2
      g3 = _G m6 m7 v3

------------------------
------------------------
-- |
-- | Replaces moreMix
-- |

--Cryptographic mixing for compression function F
xmoreMix :: W64x16 -> W64x16 -> W64x16
xmoreMix ms = moreMix0 ms0 <<>> moreMix1 ms1
    where
      (ms0,ms1) = split16 ms
      split16 :: W64x16 -> (W64x8,W64x8)
      split16 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = ((a0,a1,a2,a3,a4,a5,a6,a7),(b0,b1,b2,b3,b4,b5,b6,b7))

      moreMix0 :: W64x8 -> W64x16 -> W64x16
      moreMix0 ms0 = unpipeGRound0 . mixGRound ms0 . pipeGRound0 
        where

          pipeGRound0 :: W64x16 -> W64x4x4
          pipeGRound0 (v0,v1,v2,v3, v4,v5,v6,v7, v8,v9,v10,v11, v12,v13,v14,v15)
            = ((v0,v4,v8,v12) , (v1,v5,v9, v13) , (v2,v6,v10,v14) , (v3,v7,v11,v15))

          unpipeGRound0 :: W64x4x4 -> W64x16
          unpipeGRound0 ((v0,v4,v8,v12) , (v1,v5,v9,v13) , (v2,v6,v10,v14) , (v3,v7,v11,v15))
            =     (v0,v1,v2,v3   ,  v4,v5,v6,v7   ,  v8,v9,v10,v11  ,  v12,v13,v14,v15)

      moreMix1 :: W64x8 -> W64x16 -> W64x16
      moreMix1 ms1 = unpipeGRound1 . mixGRound ms1 . pipeGRound1 
        where

          pipeGRound1 :: W64x16 -> W64x4x4
          pipeGRound1 (v0,v1,v2,v3    ,  v4,v5,v6,v7    ,  v8,v9,v10,v11  ,  v12,v13,v14,v15)
            = ((v0,v5,v10,v15) , (v1,v6,v11,v12) , (v2,v7,v8, v13) , (v3,v4,v9, v14))

          unpipeGRound1 :: W64x4x4 -> W64x16
          unpipeGRound1 ((v0,v5,v10,v15) ,(v1,v6,v11,v12) ,(v2,v7,v8, v13) ,(v3,v4,v9, v14))
            =     (v0,v1,v2,v3, v4,v5,v6,v7, v8,v9,v10,v11, v12,v13,v14,v15)

------------------------
------------------------

type W64x16x10 = (W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16, W64x16)

xmixLoop :: W64x16x10 -> W64x16 -> W64x16
xmixLoop (ms0,ms1,ms2,ms3,ms4,ms5,ms6,ms7,ms8,ms9) =
             xmoreMix ms0 <<>> xmoreMix ms1 <<>> xmoreMix ms2 <<>> xmoreMix ms3 <<>> xmoreMix ms4
                <<>> xmoreMix ms5 <<>> xmoreMix ms6 <<>> xmoreMix ms7 <<>> xmoreMix ms8 <<>> xmoreMix ms9
                   <<>> xmoreMix ms0 <<>> xmoreMix ms1 

------------------------
------------------------

-- These are permutations. I changed the name to make it conform to the
-- RFC7693 text.
-- 
sigma0 , sigma1 , sigma2 , sigma3 , sigma4 , sigma5 , sigma6 , sigma7 , sigma8 , sigma9 :: W64x16 -> W64x16

sigma0 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
sigma1 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w14,w10,w4,w8,w9,w15,w13,w6,w1,w12,w0,w2,w11,w7,w5,w3)
sigma2 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w11,w8,w12,w0,w5,w2,w15,w13,w10,w14,w3,w6,w7,w1,w9,w4)
sigma3 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w7,w9,w3,w1,w13,w12,w11,w14,w2,w6,w5,w10,w4,w0,w15,w8)
sigma4 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w9,w0,w5,w7,w2,w4,w10,w15,w14,w1,w11,w12,w6,w8,w3,w13)
sigma5 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w2,w12,w6,w10,w0,w11,w8,w3,w4,w13,w7,w5,w15,w14,w1,w9)
sigma6 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w12,w5,w1,w15,w14,w13,w4,w10,w0,w7,w6,w3,w9,w2,w8,w11)
sigma7 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w13,w11,w7,w14,w12,w1,w3,w9,w5,w0,w15,w4,w8,w6,w2,w10)
sigma8 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w6,w15,w14,w9,w11,w3,w0,w8,w12,w2,w13,w7,w1,w4,w10,w5)
sigma9 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
              = (w10,w2,w8,w4,w7,w6,w1,w5,w15,w11,w9,w14,w3,w12,w13,w0)

------------------------
------------------------

precompute :: W64x16 -> W64x16x10
precompute ms = (sigma0 ms, sigma1 ms, sigma2 ms, sigma3 ms,
                 sigma4 ms, sigma5 ms, sigma6 ms, sigma7 ms,
                 sigma8 ms, sigma9 ms)

------------------------
------------------------

initVs :: (W128,Bit,W64x8) -> (W64x16, W64x8)
initVs (t,f,hs) = (update1213 (iv4 ^ lowhalf t) (iv5 ^ highhalf t)
                              (if f 
                               then update14 (bnot iv6) (app hs iv) 
                               else app hs iv)
                  , hs)
   where

     update1213 :: W64 -> W64 -> W64x16 -> W64x16
     update1213 n12 n13 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,n12,n13,b6,b7)

     update14 :: W64 -> W64x16 -> W64x16
     update14 n14 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,n14,b7)

     app :: W64x8 -> W64x8 -> W64x16
     app (a0,a1,a2,a3,a4,a5,a6,a7) (b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7)

     lowhalf :: W128 -> W64
     lowhalf w = slice (Proxy :: Proxy 64) w

     highhalf :: W128 -> W64
     highhalf w = rslice (Proxy :: Proxy 64) w


------------------------
------------------------

-- | This is what finalStep is with the bureaucracy boiled off.
xfinalStep :: (W64x16 , W64x8) -> W64x8
xfinalStep ( (b0,b1,b2,b3,b4,b5,b6,b7 , c0,c1,c2,c3,c4,c5,c6,c7) , (a0,a1,a2,a3,a4,a5,a6,a7) )
          = ( a0 ^ b0 ^ c0 , a1 ^ b1 ^ c1 , a2 ^ b2 ^ c2 , a3 ^ b3 ^ c3
            , a4 ^ b4 ^ c4 , a5 ^ b5 ^ c5 , a6 ^ b6 ^ c6 , a7 ^ b7 ^ c7)

------------------------
------------------------

-- xcompF :: (W64x16, (W128, Bit, W64x8)) -> W64x8
-- xcompF :: (W128, Bit, W64x8) -> W64x16 -> W64x8
goob :: (W128, Bit, W64x8) -> W64x16 -> W64x8
goob = \ y x -> ( (precompute <#> initVs) <<>>
                        (\ (ms,(vs,hs)) -> ((ms,vs),hs)) <<>> (uncurry xmixLoop <#> id)
                                                 <<>> xfinalStep ) ( x , y )

-- (precompute <#> initVs) :: (W64x16, (W128, Bit, W64x8)) -> (W64x16x10, (W64x16, W64x8))

p1 :: ((W128, Bit, W64x8), W64x16) -> ((W64x16, W64x8), W64x16x10)
p1 = (initVs <#> precompute) -- <<>> (uncurry xmixLoop <#> id) <<>> xfinalStep 

p1' :: Monad m => ((W128, Bit, W64x8), W64x16) -> m  ((W64x16, W64x8), W64x16x10)
p1' = return . p1

p2 :: ((W64x16x10, W64x16), W64x8) -> (W64x16, W64x8)
p2 = (uncurry xmixLoop <#> id)

xcompressF :: (W64x16, (W128, Bit, W64x8)) -> W64x8
xcompressF = (precompute <#> initVs) <<>> (\ (ms,(vs,hs)) -> ((ms,vs),hs)) <<>> (uncurry xmixLoop <#> id) <<>> xfinalStep

-- | (precompute <#> initVs)   :: (W64x16, (W128, Bit, W64x8)) -> (W64x16x10, (W64x16, W64x8))
-- | (uncurry xmixLoop <#> id)                                 :: ((W64x16x10, W64x16), W64x8) -> (W64x16, W64x8)
-- | xfinalStep                                                                                :: (W64x16, W64x8) -> W64x8


-- (W128 , Bit , W64x8)
------------------------
------------------------

xblake2b :: ( W128 , W64 , W64 ) -> W64x16 -> W64x8
xblake2b (ll , kk , nn) = flip (curry xcompressF) (setOffset kk ll, one, xinitHs kk nn)
   where

     setOffset :: W64 -> W128 -> W128
     setOffset kk ll = if kk == lit 0 then ll else ll + lit 128

     xinitHs :: W64 -> W64 -> W64x8
     xinitHs kk nn = update0 (iv0 ^ lit 0x01010000 ^ shiftl8 kk ^ nn) iv
       where

         update0 :: W64 -> W64x8 -> W64x8
         update0 n (_,a1,a2,a3,a4,a5,a6,a7) = (n,a1,a2,a3,a4,a5,a6,a7)
  
         shiftl8 :: W64 -> W64
         shiftl8 w = w <<. lit 8

-- blake2b_S (toW128 3) (toW64 0) (toW64 64) (toW64 0x636261, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0, toW64 0)
-- generates the example from RFC7693.

testMessage :: W64x16
testMessage = (lit 0x636261, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0)

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
