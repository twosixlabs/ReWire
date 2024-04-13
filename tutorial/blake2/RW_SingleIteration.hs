import Prelude hiding ((^),(+),(<>))
import ReWire
import ReWire.Bits (W8(..),W16(..),W32(..),W64(..),W128(..),zeroW8,eqb,isZeroW64)
import ReWire.Verilog hiding ((<>))

--Rotations:
rot64R32 :: W64 -> W64
rot64R32 (W64 h l) = (W64 l h)

rot64R24 :: W64 -> W64
rot64R24 (W64 (W32 (W16 w7 w6) (W16 w5 w4))
              (W32 (W16 w3 w2) (W16 w1 w0))) =
         (W64 (W32 (W16 w2 w1) (W16 w0 w7))
              (W32 (W16 w6 w5) (W16 w4 w3)))

rot64R16 :: W64 -> W64
rot64R16 (W64 (W32 w76 w54) (W32 w32 w10)) =
         (W64 (W32 w10 w76) (W32 w54 w32))

rot64R63 :: W64 -> W64
rot64R63 (W64   (W32    (W16 (W8 a7 a6 a5 a4 a3 a2 a1 a0) (W8 b7 b6 b5 b4 b3 b2 b1 b0))
                        (W16 (W8 c7 c6 c5 c4 c3 c2 c1 c0) (W8 d7 d6 d5 d4 d3 d2 d1 d0)))
                (W32    (W16 (W8 e7 e6 e5 e4 e3 e2 e1 e0) (W8 f7 f6 f5 f4 f3 f2 f1 f0))
                        (W16 (W8 g7 g6 g5 g4 g3 g2 g1 g0) (W8 h7 h6 h5 h4 h3 h2 h1 h0)))) = 
          W64   (W32    (W16 (W8 a6 a5 a4 a3 a2 a1 a0 b7) (W8 b6 b5 b4 b3 b2 b1 b0 c7))
                        (W16 (W8 c6 c5 c4 c3 c2 c1 c0 d7) (W8 d6 d5 d4 d3 d2 d1 d0 e7)))
                (W32    (W16 (W8 e6 e5 e4 e3 e2 e1 e0 f7) (W8 f6 f5 f4 f3 f2 f1 f0 g7))
                        (W16 (W8 g6 g5 g4 g3 g2 g1 g0 h7) (W8 h6 h5 h4 h3 h2 h1 h0 a7)))


-- Tuple types:
type W64x16 = (W64, W64, W64, W64, W64, W64, W64, W64, 
                W64, W64, W64, W64, W64, W64, W64, W64)

type W64x8 = (W64, W64, W64, W64, W64, W64, W64, W64)

data W8x64 = W8x64 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8
                   W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8
                   W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8
                   W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8 W8

type W8x8 = (W8, W8, W8, W8, W8, W8, W8, W8)

type W64x4 = (W64, W64, W64, W64)

type W64x4x4 = (W64x4, W64x4, W64x4, W64x4)

type W64x16x10 = (W64x16, W64x16, W64x16, W64x16,  W64x16, W64x16, W64x16, W64x16,  W64x16, W64x16)

--wordLen :: Int = 64
--r :: Int
bb :: W128
--r1 :: Int = 32
--r2 :: Int = 24
--r3 :: Int = 16
--r4 :: Int = 63
--r = 12
bb = lit 128

-- The 10 permutations:
perm0 :: W64x16 -> W64x16
perm1 :: W64x16 -> W64x16
perm2 :: W64x16 -> W64x16
perm3 :: W64x16 -> W64x16
perm4 :: W64x16 -> W64x16
perm5 :: W64x16 -> W64x16
perm6 :: W64x16 -> W64x16
perm7 :: W64x16 -> W64x16
perm8 :: W64x16 -> W64x16
perm9 :: W64x16 -> W64x16
perm0 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15)
perm1 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w14,w10,w4,w8,w9,w15,w13,w6,w1,w12,w0,w2,w11,w7,w5,w3)
perm2 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w11,w8,w12,w0,w5,w2,w15,w13,w10,w14,w3,w6,w7,w1,w9,w4)
perm3 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w7,w9,w3,w1,w13,w12,w11,w14,w2,w6,w5,w10,w4,w0,w15,w8)
perm4 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w9,w0,w5,w7,w2,w4,w10,w15,w14,w1,w11,w12,w6,w8,w3,w13)
perm5 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w2,w12,w6,w10,w0,w11,w8,w3,w4,w13,w7,w5,w15,w14,w1,w9)
perm6 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w12,w5,w1,w15,w14,w13,w4,w10,w0,w7,w6,w3,w9,w2,w8,w11)
perm7 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w13,w11,w7,w14,w12,w1,w3,w9,w5,w0,w15,w4,w8,w6,w2,w10)
perm8 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w6,w15,w14,w9,w11,w3,w0,w8,w12,w2,w13,w7,w1,w4,w10,w5)
perm9 (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15) = (w10,w2,w8,w4,w7,w6,w1,w5,w15,w11,w9,w14,w3,w12,w13,w0)

split16 :: W64x16 -> (W64x8,W64x8)
split16 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = ((a0,a1,a2,a3,a4,a5,a6,a7),(b0,b1,b2,b3,b4,b5,b6,b7))

map3x8doubleXor :: W64x8 -> W64x8 -> W64x8 -> W64x8
map3x8doubleXor (a0,a1,a2,a3,a4,a5,a6,a7) (b0,b1,b2,b3,b4,b5,b6,b7) (c0,c1,c2,c3,c4,c5,c6,c7)
   = ((a0 ^ b0) ^ c0, (a1 ^ b1) ^ c1, (a2 ^ b2) ^ c2, (a3 ^ b3) ^ c3, 
      (a4 ^ b4) ^ c4, (a5 ^ b5) ^ c5, (a6 ^ b6) ^ c6, (a7 ^ b7) ^ c7)

lowhalf :: W128 -> W64
lowhalf (W128 h l) = l

highhalf :: W128 -> W64
highhalf (W128 h l) = h

shiftl8 :: W64 -> W64
shiftl8 (W64 (W32 (W16 w7 w6) (W16 w5 w4))
             (W32 (W16 w3 w2) (W16 w1 w0)))
      = (W64 (W32 (W16 w6 w5) (W16 w4 w3))
             (W32 (W16 w2 w1) (W16 w0 zeroW8)))


--Initialization of iv

iv0 :: W64
iv1 :: W64
iv2 :: W64
iv3 :: W64
iv4 :: W64
iv5 :: W64
iv6 :: W64
iv7 :: W64
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


--Helper functions for F
app :: W64x8 -> W64x8 -> W64x16
app (a0,a1,a2,a3,a4,a5,a6,a7) (b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7)

update1213 :: W64 -> W64 -> W64x16 -> W64x16
update1213 n12 n13 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,n12,n13,b6,b7)

update14 :: W64 -> W64x16 -> W64x16
update14 n14 (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7) = (a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,n14,b7)

update0 :: W64 -> W64x8 -> W64x8
update0 n (a0,a1,a2,a3,a4,a5,a6,a7) = (n,a1,a2,a3,a4,a5,a6,a7)

--The output is a specified number of bytes, so we convert a W64x8 to a byte list 
w64toW8x8 :: W64 -> W8x8
w64toW8x8 (W64  (W32 (W16 w7 w6) (W16 w5 w4))
                (W32 (W16 w3 w2) (W16 w1 w0))) = (w7,w6,w5,w4,w3,w2,w1,w0)

w64x8toW8x64 :: W64x8 -> W8x64
w64x8toW8x64 (w0,w1,w2,w3,w4,w5,w6,w7)
  = W8x64  x_0  x_1  x_2  x_3  x_4  x_5  x_6  x_7  x_8  x_9 x_10 x_11 x_12 x_13 x_14 x_15
           x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29 x_30 x_31
           x_32 x_33 x_34 x_35 x_36 x_37 x_38 x_39 x_40 x_41 x_42 x_43 x_44 x_45 x_46 x_47
           x_48 x_49 x_50 x_51 x_52 x_53 x_54 x_55 x_56 x_57 x_58 x_59 x_60 x_61 x_62 x_63
   where
      (x_0, x_1, x_2, x_3, x_4, x_5, x_6, x_7)     = w64toW8x8 w0
      (x_8, x_9, x_10, x_11, x_12, x_13, x_14, x_15) = w64toW8x8 w1
      (x_16, x_17, x_18, x_19, x_20, x_21, x_22, x_23) = w64toW8x8 w2
      (x_24, x_25, x_26, x_27, x_28, x_29, x_30, x_31) = w64toW8x8 w3
      (x_32, x_33, x_34, x_35, x_36, x_37, x_38, x_39) = w64toW8x8 w4
      (x_40, x_41, x_42, x_43, x_44, x_45, x_46, x_47) = w64toW8x8 w5
      (x_48, x_49, x_50, x_51, x_52, x_53, x_54, x_55) = w64toW8x8 w6
      (x_56, x_57, x_58, x_59, x_60, x_61, x_62, x_63) = w64toW8x8 w7

rev64bytes :: W8x64 -> W8x64
rev64bytes (W8x64  x_0  x_1  x_2  x_3  x_4  x_5  x_6  x_7  x_8  x_9 x_10 x_11 x_12 x_13 x_14 x_15
                   x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29 x_30 x_31
                   x_32 x_33 x_34 x_35 x_36 x_37 x_38 x_39 x_40 x_41 x_42 x_43 x_44 x_45 x_46 x_47
                   x_48 x_49 x_50 x_51 x_52 x_53 x_54 x_55 x_56 x_57 x_58 x_59 x_60 x_61 x_62 x_63)
      = W8x64  x_63 x_62 x_61 x_60 x_59 x_58 x_57 x_56 x_55 x_54 x_53 x_52 x_51 x_50 x_49 x_48
               x_47 x_46 x_45 x_44 x_43 x_42 x_41 x_40 x_39 x_38 x_37 x_36 x_35 x_34 x_33 x_32
               x_31 x_30 x_29 x_28 x_27 x_26 x_25 x_24 x_23 x_22 x_21 x_20 x_19 x_18 x_17 x_16
               x_15 x_14 x_13 x_12 x_11 x_10 x_9  x_8  x_7  x_6  x_5  x_4  x_3  x_2  x_1  x_0


take64 :: W64 -> W8x64 -> W8x64
take64 _ w = w
-- to be changed




------------------------------------------------
-- BLAKE2b, composition-representation version
------------------------------------------------

--Mixing function G
mixG :: W64x4 -> W64 -> W64-> W64x4 
mixG (vsa,vsb,vsc,vsd) x y = (vsa2,vsb2,vsc2,vsd2)
    where
      vsa1 = (vsa + vsb) + x --addition for W64 is automatically mod 2^64
      vsd1 = rot64R32 (vsd ^ vsa1)
      vsc1 = vsc + vsd1
      vsb1 = rot64R24 (vsb ^ vsc1)
      
      vsa2 = (vsa1 + vsb1) + y
      vsd2 = rot64R16 (vsd1 ^ vsa2)
      vsc2 = vsc1 + vsd2
      vsb2 = rot64R63 (vsb1 ^ vsc2)

mixGRound :: W64x8 -> W64x4x4 -> W64x4x4
mixGRound (m0,m1,m2,m3, m4,m5,m6,m7) (vs0,vs1,vs2,vs3)
    = (g0,g1,g2,g3)
    where
      g0 = mixG vs0 m0  m1
      g1 = mixG vs1 m2  m3
      g2 = mixG vs2 m4  m5
      g3 = mixG vs3 m6  m7

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

-- Question to ponder: Could we include the unpipe/pipe as part of the prepermutation?

--Cryptographic mixing for compression function F
moreMix :: W64x16 -> W64x16 -> W64x16
moreMix ms = 
      unpipeGRound1 . mixGRound ms1 . pipeGRound1 
    . unpipeGRound0 . mixGRound ms0 . pipeGRound0
    where
      (ms0,ms1) = split16 ms

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
finalStep (vs,hs) = map3x8doubleXor hs vs1 vs2
  where
    (vs1,vs2) = split16 vs

initHs :: W64 -> W64 -> W64x8
initHs kk nn = update0 (iv0 ^ (lit 0x01010000) ^ shiftl8 kk ^ nn) iv

initVs :: (W128,Bit,W64x8) -> (W64x16, W64x8)
initVs (t,f,hs) = (update1213 (iv4 ^ lowhalf t) (iv5 ^ highhalf t)
                              (if (eqb f S) 
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
 {- similar to: 
   finalStep (mixLoop (prepermute ms) (initVs (t,f,hs)), hs)
 but initVs also passes hs through for convenience

 question: do we have to define the lambda used here?
 -}



setOffset :: W64 -> W128 -> W128
setOffset kk ll = if (isZeroW64 kk) then ll else ll + bb

--The multiblock aspects of Blake2 are commented out for now to avoid causing issues for the rewire compiler

--passing input to F
--processData :: W64x8 -> [W64x16] -> W128 -> W128 -> W64x8
--processData hs ds i1bb offset 
--    | null ds = hs 
--    | length ds == 1 
--    = compressF (head ds,(offset,S,hs)) 
--    | otherwise
--    = processData (compressF (head ds,(i1bb,C,hs))) (tail ds) (i1bb+bb) offset
{-
   If the input grants us
     - a counter for the amount of input to be consumed (+1 if there's a key) and
     - an offset equal to ll mod bb
   Then processData could just become
    | null counter = compressF (head ds, i1bb+offset,S,hs)
    | otherwise = processData (compressF (head ds i1bb,C,hs) (tail ds) (i1bb+bb) offset)
   -}


--BLAKE2b cryptographic hash algorithm
--blake2b :: [W64x16] -> W128 -> W64 -> W64 -> W64x8
--blake2b ds ll kk nn = hs where
--                      --take64 nn (rev64bytes (w64x8toW8x64 hs)) where
--    hs = processData (initHs kk nn) ds bb (setOffset kk ll)
-- The output type in Blake2b.hs was W64x8. We may be changing to W8x64 to give the output as stream of byte-words.
-- It's easier to write out correctHash in the W64x8 format for testing.


--------------------------------------------------------------
-- single-iteration version of blake2b for easy rewire-izing
--------------------------------------------------------------

blake2b_S :: W128 -> W64 -> W64 -> W64x16 -> W64x8
blake2b_S ll kk nn = flip (curry compressF) (setOffset kk ll, S, initHs kk nn)


-- The device definition from Bill's Salsa20 code
type Dev i o = ReT i o I ()

-- Parameters:
ll :: W128
ll = lit 3
kk :: W64
kk = lit 0
nn :: W64
nn = lit 64

--A baseline device implementing the pure function version of single-block Blake2b
--following Bill's first Salsa20 example

blake2b_S_dev :: W64x16 -> Dev W64x16 W64x8
blake2b_S_dev i = signal (blake2b_S ll kk nn i) >>= blake2b_S_dev

start :: Dev W64x16 W64x8
start = blake2b_S_dev (lit 0, lit 0, lit 0, lit 0, 
                        lit 0, lit 0, lit 0, lit 0,
                        lit 0, lit 0, lit 0, lit 0,
                        lit 0, lit 0, lit 0, lit 0)
