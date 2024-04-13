{-# LANGUAGE DataKinds #-}
-- |
-- | This is a refactoring of the reference semantics
-- | for Blake2b so that it closely resembles the
-- | RFC 7693 pseudocode.
-- |

import Prelude hiding ((^), (+), (==), (&&) , (++))
import ReWire hiding (error)
import ReWire.Bits
import ReWire.Vectors

-----------------------------
-- Definitions and helpers
-----------------------------

type W64  = W 64
type W128 = W 128
data Reg  = V0 | V1 |  V2 |  V3 |  V4 |  V5 |  V6 |  V7 -- working vectors v[0..15]
          | V8 | V9 | V10 | V11 | V12 | V13 | V14 | V15
          | M0 | M1 |  M2 |  M3 |  M4 |  M5 |  M6 |  M7 -- message buffer  m[0..15]
          | M8 | M9 | M10 | M11 | M12 | M13 | M14 | M15
          | H0 | H1 |  H2 |  H3 |  H4 |  H5 |  H6 |  H7 -- hash state      h[0..7]

data RegFile = RegFile
                 { v0 , v1 ,  v2 ,  v3 ,  v4 ,  v5 ,  v6 ,  v7
                 , v8 , v9 , v10 , v11 , v12 , v13 , v14 , v15
                 , m0 , m1 ,  m2 ,  m3 ,  m4 ,  m5 ,  m6 ,  m7
                 , m8 , m9 , m10 , m11 , m12 , m13 , m14 , m15
                 , h0 , h1 ,  h2 ,  h3 ,  h4 ,  h5 ,  h6 ,  h7
                   :: W64 }

type M = StateT RegFile Identity
          
trReg :: Reg -> RegFile -> W64
trReg r = case r of       
               ----
               V0  -> v0
               V1  -> v1
               V2  -> v2
               V3  -> v3
               V4  -> v4
               V5  -> v5
               V6  -> v6
               V7  -> v7
               V8  -> v8
               V9  -> v9
               V10 -> v10
               V11 -> v11
               V12 -> v12
               V13 -> v13
               V14 -> v14
               V15 -> v15
               ----
               M0  -> m0
               M1  -> m1
               M2  -> m2
               M3  -> m3
               M4  -> m4
               M5  -> m5
               M6  -> m6
               M7  -> m7
               M8  -> m8
               M9  -> m9
               M10 -> m10
               M11 -> m11
               M12 -> m12
               M13 -> m13
               M14 -> m14
               M15 -> m15
               ----
               H0  -> h0
               H1  -> h1
               H2  -> h2
               H3  -> h3
               H4  -> h4
               H5  -> h5
               H6  -> h6
               H7  -> h7

-- {-# INLINE readReg #-}
readReg :: Reg -> M W64
readReg v = do
             rf <- get
             return (trReg v rf)

-- {-# INLINE setReg #-}
setReg :: Reg -> W64 -> M ()
setReg r w = do
               rf <- get
               case r of
                 ----
                 V0  -> put $ rf { v0 = w }
                 V1  -> put $ rf { v1 = w }
                 V2  -> put $ rf { v2 = w }
                 V3  -> put $ rf { v3 = w }
                 V4  -> put $ rf { v4 = w }
                 V5  -> put $ rf { v5 = w }
                 V6  -> put $ rf { v6 = w }
                 V7  -> put $ rf { v7 = w }
                 V8  -> put $ rf { v8 = w }
                 V9  -> put $ rf { v9 = w }
                 V10 -> put $ rf { v10 = w }
                 V11 -> put $ rf { v11 = w }
                 V12 -> put $ rf { v12 = w }
                 V13 -> put $ rf { v13 = w }
                 V14 -> put $ rf { v14 = w }
                 V15 -> put $ rf { v15 = w }
                 ----
                 M0  -> put $ rf { m0 = w }
                 M1  -> put $ rf { m1 = w }
                 M2  -> put $ rf { m2 = w }
                 M3  -> put $ rf { m3 = w }
                 M4  -> put $ rf { m4 = w }
                 M5  -> put $ rf { m5 = w }
                 M6  -> put $ rf { m6 = w }
                 M7  -> put $ rf { m7 = w }
                 M8  -> put $ rf { m8 = w }
                 M9  -> put $ rf { m9 = w }
                 M10 -> put $ rf { m10 = w }
                 M11 -> put $ rf { m11 = w }
                 M12 -> put $ rf { m12 = w }
                 M13 -> put $ rf { m13 = w }
                 M14 -> put $ rf { m14 = w }
                 M15 -> put $ rf { m15 = w }
                 ----
                 H0  -> put $ rf { h0 = w }
                 H1  -> put $ rf { h1 = w }
                 H2  -> put $ rf { h2 = w }
                 H3  -> put $ rf { h3 = w }
                 H4  -> put $ rf { h4 = w }
                 H5  -> put $ rf { h5 = w }
                 H6  -> put $ rf { h6 = w }
                 H7  -> put $ rf { h7 = w }

{-# INLINE (<==) #-}
(<==) :: Reg -> M (W 64) -> M ()
w <== e = e >>= setReg w

regfile0 :: RegFile
regfile0 = RegFile {  v0 = lit 0
                   ,  v1 = lit 0 
                   ,  v2 = lit 0 
                   ,  v3 = lit 0 
                   ,  v4 = lit 0 
                   ,  v5 = lit 0 
                   ,  v6 = lit 0
                   ,  v7 = lit 0
                   ,  v8 = lit 0
                   ,  v9 = lit 0
                   , v10 = lit 0 
                   , v11 = lit 0
                   , v12 = lit 0
                   , v13 = lit 0
                   , v14 = lit 0
                   , v15 = lit 0
                   ,  m0 = lit 0
                   ,  m1 = lit 0 
                   ,  m2 = lit 0 
                   ,  m3 = lit 0 
                   ,  m4 = lit 0 
                   ,  m5 = lit 0 
                   ,  m6 = lit 0
                   ,  m7 = lit 0
                   ,  m8 = lit 0
                   ,  m9 = lit 0
                   , m10 = lit 0 
                   , m11 = lit 0
                   , m12 = lit 0
                   , m13 = lit 0
                   , m14 = lit 0
                   , m15 = lit 0
                   ,  h0 = lit 0
                   ,  h1 = lit 0 
                   ,  h2 = lit 0 
                   ,  h3 = lit 0 
                   ,  h4 = lit 0 
                   ,  h5 = lit 0 
                   ,  h6 = lit 0
                   ,  h7 = lit 0 }

----------------------------------
-- 3.1.1. Mixing Function G     -- 
----------------------------------

-- | 2.1. G Rotation constants
_R1 , _R2 , _R3 , _R4 :: W64
_R1 = lit 32
_R2 = lit 24
_R3 = lit 16
_R4 = lit 63

_G :: Reg -> Reg -> Reg -> Reg -> Reg -> Reg -> M ()
_G a b c d x y    = do
       add3 a b x >>= setReg a
       xorrotR d a _R1 >>= setReg d
       add c d >>= setReg c
       xorrotR b c _R2 >>= setReg b
       add3 a b y >>= setReg a
       xorrotR d a _R3 >>= setReg d
       add c d >>= setReg c
       xorrotR b c _R4 >>= setReg b
   where
     add :: Reg -> Reg -> M W64
     add v1 v2    = do
                      a <- readReg v1
                      b <- readReg v2
                      return (a + b)
     -- |
     -- | Need a theorem: forall w :: W 64, w `mod` (2 ^^ 64) == w.
     -- |
     add3 :: Reg -> Reg -> Reg -> M W64
     add3 v1 v2 z = do
                      a <- readReg v1
                      b <- readReg v2
                      x <- readReg z
                      return (a + b + x)

     xorrotR :: Reg -> Reg -> W 64 -> M (W 64)
     xorrotR v1 v2 rc = do
                           va <- readReg v1
                           vb <- readReg v2
                           return (rotR rc (va ^ vb))


----------------------------------
-- 3.2. Compression Function G  -- 
----------------------------------

iv0 , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7 :: W64
iv0 = lit 0x6a09e667f3bcc908
iv1 = lit 0xbb67ae8584caa73b
iv2 = lit 0x3c6ef372fe94f82b
iv3 = lit 0xa54ff53a5f1d36f1
iv4 = lit 0x510e527fade682d1
iv5 = lit 0x9b05688c2b3e6c1f
iv6 = lit 0x1f83d9abfb41bd6b
iv7 = lit 0x5be0cd19137e2179
                     
-- These are permutations. I changed the name to make it conform to the
-- RFC7693 text.
-- 
type Reg16 = (Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg)

sigma0 , sigma1 , sigma2 , sigma3 , sigma4 , sigma5 , sigma6 , sigma7 , sigma8 , sigma9 :: Reg16

sigma0 = (M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15)
sigma1 = (M14,M10,M4,M8,M9,M15,M13,M6,M1,M12,M0,M2,M11,M7,M5,M3)
sigma2 = (M11,M8,M12,M0,M5,M2,M15,M13,M10,M14,M3,M6,M7,M1,M9,M4)
sigma3 = (M7,M9,M3,M1,M13,M12,M11,M14,M2,M6,M5,M10,M4,M0,M15,M8)
sigma4 = (M9,M0,M5,M7,M2,M4,M10,M15,M14,M1,M11,M12,M6,M8,M3,M13)
sigma5 = (M2,M12,M6,M10,M0,M11,M8,M3,M4,M13,M7,M5,M15,M14,M1,M9)
sigma6 = (M12,M5,M1,M15,M14,M13,M4,M10,M0,M7,M6,M3,M9,M2,M8,M11)
sigma7 = (M13,M11,M7,M14,M12,M1,M3,M9,M5,M0,M15,M4,M8,M6,M2,M10)
sigma8 = (M6,M15,M14,M9,M11,M3,M0,M8,M12,M2,M13,M7,M1,M4,M10,M5)
sigma9 = (M10,M2,M8,M4,M7,M6,M1,M5,M15,M11,M9,M14,M3,M12,M13,M0)

-- |
-- | The body of the FOR loop in definition of F.
-- |
-- message_permutation :: Reg16 -> Dev ()
message_permutation :: Reg16 -> M ()
message_permutation (ms0,ms1,ms2,ms3,ms4,ms5,ms6,ms7,ms8,ms9,ms10,ms11,ms12,ms13,ms14,ms15) = 
               do
                 _G V0 V4  V8 V12 ms0 ms1
                 _G V1 V5  V9 V13 ms2 ms3
                 _G V2 V6 V10 V14 ms4 ms5
                 _G V3 V7 V11 V15 ms6 ms7
                 _G V0 V5 V10 V15 ms8 ms9
                 _G V1 V6 V11 V12 ms10 ms11
                 _G V2 V7 V8  V13 ms12 ms13
                 _G V3 V4 V9  V14 ms14 ms15

-- |
-- | These are the staging operators.
-- |

--
-- Most general form. Not used.
--
-- mkcyc :: Monad m => (i -> a -> m (o, b)) -> (i , a) -> ReacT i (Maybe o) m (i , b)
-- mkcyc f (i , a) = do
--                     (o , b) <- lift (f i a)
--                     i'      <- signal Nothing
--                     return (i' , b)

-- {-# INLINE cycJ_ #-}
-- cycJ_ :: M o -> ReacT i (Maybe o) M i
-- cycJ_ x = do
--             o <- lift x
--             signal (Just o)

-- {-# INLINE cycN #-}
-- cycN :: Monad m => (i -> a -> m b) -> (i , a) -> ReacT i (Maybe o) m (i , b)
-- cycN f (i , a) = do
--                    b <- lift (f i a)
--                    i' <- signal Nothing
--                    return (i' , b)

{-# INLINE cycN_ #-}
cycN_ :: M () -> Dev (Inp (W 64, W 64 , W 64 , W 64))
cycN_ x        = do
                   lift x
                   i' <- signal Nothing
                   return i' 

{-# INLINE _cycN #-}
_cycN :: M () -> Dev ()
_cycN x        = do
                   lift x
                   signal Nothing
                   return ()
  
crypto_mixing :: M ()
crypto_mixing = do
                  message_permutation sigma0
                  message_permutation sigma1
                  message_permutation sigma2
                  message_permutation sigma3
                  message_permutation sigma4
                  message_permutation sigma5
                  message_permutation sigma6
                  message_permutation sigma7
                  message_permutation sigma8
                  message_permutation sigma9
                  message_permutation sigma0
                  message_permutation sigma1

-- crypto_mixing = do
--                    cycN_ $ do message_permutation sigma0
--                               message_permutation sigma1
--                    cycN_ $ do message_permutation sigma2
--                               message_permutation sigma3
--                    cycN_ $ do message_permutation sigma4
--                               message_permutation sigma5
--                    cycN_ $ do message_permutation sigma6
--                               message_permutation sigma7
--                    cycN_ $ do message_permutation sigma8
--                               message_permutation sigma9
--                    cycN_ $ do message_permutation sigma0
--                               message_permutation sigma1

--
-- Ver1 crypto_mixing
--
-- crypto_mixing :: Dev (Inp (W 64, W 64 , W 64 , W 64))
-- crypto_mixing = do
--                    cycN_ $ do
--                              message_permutation sigma0
--                              message_permutation sigma1
--                              message_permutation sigma2
--                              message_permutation sigma3
--                              message_permutation sigma4
--                              message_permutation sigma5
--                              message_permutation sigma6
--                              message_permutation sigma7
--                              message_permutation sigma8
--                              message_permutation sigma9
--                              message_permutation sigma0
--                              message_permutation sigma1


_F :: W128 -> Bit -> Dev ()
_F t f = do
           cycN_ $ do
             init_work_vector
             V12 <== do { w <- readReg V12 ; return $ w ^ lowword t }
             V13 <== do { w <- readReg V13 ; return $ w ^ highword t }
             if f then
                    do
                      V14 <== do { w <- readReg V14 ; return $ w ^ lit 0xffffffffffffffff }
                  else
                    return ()
           _cycN crypto_mixing
           _cycN xor_two_halves

  where

     init_work_vector :: M ()
     init_work_vector = do
                     (readReg H0 >>= setReg V0)
                     (readReg H1 >>= setReg V1)
                     (readReg H2 >>= setReg V2)
                     (readReg H3 >>= setReg V3)
                     (readReg H4 >>= setReg V4)
                     (readReg H5 >>= setReg V5)
                     (readReg H6 >>= setReg V6)
                     (readReg H7 >>= setReg V7)
                     setReg V8 iv0
                     setReg V9 iv1
                     setReg V10 iv2
                     setReg V11 iv3
                     setReg V12 iv4
                     setReg V13 iv5
                     setReg V14 iv6
                     setReg V15 iv7
                     -- V0  <== readReg H0
                     -- V1  <== readReg H1
                     -- V2  <== readReg H2
                     -- V3  <== readReg H3
                     -- V4  <== readReg H4
                     -- V5  <== readReg H5
                     -- V6  <== readReg H6
                     -- V7  <== readReg H7
                     -- V8  <== return iv0
                     -- V9  <== return iv1
                     -- V10 <== return iv2
                     -- V11 <== return iv3
                     -- V12 <== return iv4
                     -- V13 <== return iv5
                     -- V14 <== return iv6
                     -- V15 <== return iv7

     xor_two_halves :: M ()
     xor_two_halves = do
                   xor3 H0 V0 V8  >>= setReg H0
                   xor3 H1 V1 V9  >>= setReg H1
                   xor3 H2 V2 V10 >>= setReg H2
                   xor3 H3 V3 V11 >>= setReg H3
                   xor3 H4 V4 V12 >>= setReg H4
                   xor3 H5 V5 V13 >>= setReg H5
                   xor3 H6 V6 V14 >>= setReg H6
                   xor3 H7 V7 V15 >>= setReg H7
                   -- H0 <== xor3 H0 V0 V8  
                   -- H1 <== xor3 H1 V1 V9
                   -- H2 <== xor3 H2 V2 V10
                   -- H3 <== xor3 H3 V3 V11
                   -- H4 <== xor3 H4 V4 V12
                   -- H5 <== xor3 H5 V5 V13
                   -- H6 <== xor3 H6 V6 V14
                   -- H7 <== xor3 H7 V7 V15
                     where
                       xor3 :: Reg -> Reg -> Reg -> M (W 64)
                       xor3 r1 r2 r3 = do
                         w1 <- readReg r1
                         w2 <- readReg r2
                         w3 <- readReg r3
                         return $ w1 ^ w2 ^ w3
    
     lowword :: W128 -> W64
     lowword w = slice (Proxy :: Proxy 64) w

     highword :: W128 -> W64
     highword w = rslice (Proxy :: Proxy 64) w

type W64x4  = ( W 64 , W 64 , W 64 , W 64 )
type W64x8  = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )
type W64x16 = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64
              , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )

initM :: W64x16 -> M ()
initM (w0 , w1 , w2 , w3 , w4 , w5 , w6 , w7 , w8 , w9 , w10 , w11 , w12 , w13 , w14 , w15 ) = do
                         setReg M0 w0
                         setReg M1 w1
                         setReg M2 w2
                         setReg M3 w3
                         setReg M4 w4
                         setReg M5 w5
                         setReg M6 w6
                         setReg M7 w7
                         setReg M8 w8
                         setReg M9 w9
                         setReg M10 w10
                         setReg M11 w11
                         setReg M12 w12
                         setReg M13 w13
                         setReg M14 w14
                         setReg M15 w15
            
-- readH :: M W64x8
-- readH :: ReacT i (Maybe W64x8) M i
-- readH :: ReacT i (Maybe (W64, W64, W64, W64, W64, W64, W64, W64)) M i
readH :: Dev W64x8
readH = lift $ do
  h0 <- readReg H0
  h1 <- readReg H1
  h2 <- readReg H2
  h3 <- readReg H3
  h4 <- readReg H4
  h5 <- readReg H5
  h6 <- readReg H6
  h7 <- readReg H7
  return (h0 , h1 , h2 , h3 , h4 , h5 , h6 , h7)

-- |
-- | This version is simplified assuming that dd==1 && kk==0
-- |

-- 
-- Number of bytes in a block. For Blake2b, it's 128
-- 
bb :: W 128
bb = lit 128

data Inp a     = Start a | DC a | Q0 a | Q1 a | Q2 a | Q3 a | Args a 

type Dev = ReacT (Inp (W 64, W 64 , W 64 , W 64)) (Maybe W64x8) M

blake2b :: Inp (W 64, W 64 , W 64 , W 64) -> Dev ()
blake2b (Q0 (w0 , w1 , w2 , w3)) = do
     i <- cycN_ $ do
                    setReg M0 w0
                    setReg M1 w1
                    setReg M2 w2
                    setReg M3 w3
     blake2b i
blake2b (Q1 (w4 , w5 , w6 , w7)) = do
     i <- cycN_ $ do
                    setReg M4 w4
                    setReg M5 w5
                    setReg M6 w6
                    setReg M7 w7
     blake2b i
blake2b (Q2 (w8 , w9 , w10 , w11)) = do
     i <- cycN_ $ do
                    setReg M8 w8
                    setReg M9 w9
                    setReg M10 w10
                    setReg M11 w11
     blake2b i
blake2b (Q3 (w12 , w13 , w14 , w15)) = do
     lift $ do
       setReg M12 w12
       setReg M13 w13
       setReg M14 w14
       setReg M15 w15
     i <- signal Nothing
     blake2b i
blake2b (Args (ll_hi , ll_lo , kk , nn)) = do
     v <-_BLAKE2b (ll_hi ++ ll_lo) kk nn
     i <- signal (Just v)
     blake2b i

_BLAKE2b :: W 128 -> W 64 -> W 64 {- -> W64x16 -} -> Dev W64x8
_BLAKE2b ll kk nn {- d0 -} = do
                     
                     cycN_ $ do
                               setReg H0 iv0  -- Initialization Vector.
                               setReg H1 iv1
                               setReg H2 iv2
                               setReg H3 iv3
                               setReg H4 iv4
                               setReg H5 iv5
                               setReg H6 iv6
                               setReg H7 iv7
                               -- H0 <== return iv0  -- Initialization Vector.
                               -- H1 <== return iv1
                               -- H2 <== return iv2
                               -- H3 <== return iv3
                               -- H4 <== return iv4
                               -- H5 <== return iv5
                               -- H6 <== return iv6
                               -- H7 <== return iv7

                            -- Parameter block p[0]
                               v <-  do
                                        h0 <- readReg H0
                                        return $ h0 ^ lit 0x01010000 ^ (kk <<. lit 8) ^ nn
                               setReg H0 v

                     if kk == lit 0
                       then
                         _F ll True
                       else
                         _F (ll + bb) True

                     readH

start :: ReacT (Inp W64x4) (Maybe W64x8) Identity ()
start = extrude (signal Nothing >>= blake2b) regfile0

{-
  
-- |
-- | Testing
-- |

newtype T8 a  = T8 (a , a , a , a , a , a , a , a )

instance ShowHex a => Show (T8 a) where
  show (T8 (a0 , a1 , a2 , a3 , a4 , a5 , a6 , a7 )) = "(" Prelude.++ xshow a0 Prelude.++ " , " Prelude.++
                                                              xshow a1 Prelude.++ " , " Prelude.++
                                                              xshow a2 Prelude.++ " , " Prelude.++
                                                              xshow a3 Prelude.++ " , " Prelude.++
                                                              xshow a4 Prelude.++ " , " Prelude.++
                                                              xshow a5 Prelude.++ " , " Prelude.++
                                                              xshow a6 Prelude.++ " , " Prelude.++
                                                              xshow a7 Prelude.++ ")"


ex1 :: T8 W64
ex1 =  T8 $ fst $ runIdentity (runStateT (_BLAKE2b (lit 3) (lit 0) (lit 64) testMessage) undef0)
   where
     testMessage :: W64x16
     testMessage = (lit 0x636261, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0, lit 0)

-- Want : (0x0d4d1c983fa580ba , 0xe9f6129fb697276a , 0xb7c45a68142f214c , 0xd1a2ffdb6fbb124b , 0x2d79ab2a39c5877d , 0x95cc3345ded552c2 , 0x5a92f1dba88ad318 , 0x239900d4ed8623b9)
-- Now :  (0x0d4d1c983fa580ba , 0xe9f6129fb697276a , 0xb7c45a68142f214c , 0xd1a2ffdb6fbb124b , 0x2d79ab2a39c5877d , 0x95cc3345ded552c2 , 0x5a92f1dba88ad318 , 0x239900d4ed8623b9)

-- Got  : (0x23844df1a22fbf9c , 0x63cbb86d473f6a59 , 0x11adc41ac840c90c , 0x7dbf3bcfb3da3ac4 , 0xfe58b1429c5da0e8 , 0x6419e293b3eb0476 , 0xf65cb2fbab29d9a3 , 0x5187c1e427630f99)
-}

---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------
--                    Junkyard is below.
---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------

{-
-- loadargs :: (W 64 , W 64) -> 

-- data Inp = InputBytes (W 64 , W 64)  -- ll
--          | BlockBytes (W 64 , W 64)  -- 

data Inp a = Start a | Val a | DC a

blakedev :: Inp (W 64 , W 64) ->
            ReacT (Inp (W 64, W 64)) (Maybe o) M (W 128 , W 64 , W 64 , W64x16 )
blakedev (Start (llh , lll)) = do
                                 n1                  <- signal Nothing
                                 (n2 , (kk , nn))    <- param n1
                                 (n3 , (d0 , d1))    <- param n2
                                 (n4 , (d2 , d3))    <- param n3
                                 (n5 , (d4 , d5))    <- param n4
                                 (n6 , (d6 , d7))    <- param n5
                                 (n7 , (d8 , d9))    <- param n6
                                 (n8 , (d10 , d11))  <- param n7
                                 (n9 , (d12 , d13))  <- param n8
                                 (n10 , (d14 , d15)) <- param n9
                                 return ( llh ++ lll , kk , nn
                                        , (d0 , d1 , d2 , d3 , d4 , d5 , d6 , d7
                                        ,  d8 , d9 , d10 , d11 , d12 , d13 , d14 , d15) )
blakedev _                   = signal Nothing >>= blakedev

-- param :: Inp (W 64, W 64) -> ReacT (Inp (W 64, W 64)) (Maybe o) M (Inp (W 64, W 64), (W 64, W 64))
param :: Inp (W 64, W 64) -> ReacT (Inp (W 64, W 64)) (Maybe o) M (Inp (W 64, W 64), (W 64, W 64))
param (Val (w1 , w2))       = do
                                 n <- signal Nothing
                                 return (n , (w1 , w2))
-- param _                     = signal Nothing >>= blakedev

param :: Inp (W 64 , W 64) -> Dev (W 64, W 64)
param (Val (w1 , w2))       = return (w1 , w2)
-- param i                     = preamble i

nextarg :: Dev (W 64, W 64)
nextarg = signal Nothing >>= param

-- blake2b :: Dev (W 64, W 64)
blake2b                   = signal Nothing >>= preamble -- >>= _BLAKE2b

-- preamble :: Inp (W 64, W 64) -> Dev (W 64, W 64)
-- preamble :: Inp (W 64 , W 64) -> Dev (W 128, W 64, W 64,
--                                        (W 64, W 64, W 64, W 64, W 64, W 64, W 64, W 64, W 64, W 64, W 64,
--                                         W 64, W 64, W 64, W 64, W 64))
preamble (Start (llh , lll)) = do
                                 (kk , nn)   <- nextarg
                                 (d0 , d1)   <- nextarg
                                 (d2 , d3)   <- nextarg
                                 (d4 , d5)   <- nextarg
                                 (d6 , d7)   <- nextarg
                                 (d8 , d9)   <- nextarg
                                 (d10 , d11) <- nextarg
                                 (d12 , d13) <- nextarg
                                 (d14 , d15) <- nextarg
                                 return ( llh ++ lll , kk , nn
                                        , (d0 , d1 , d2 , d3 , d4 , d5 , d6 , d7
                                        ,  d8 , d9 , d10 , d11 , d12 , d13 , d14 , d15) )

-- preamble _                   = blake2b

-}
