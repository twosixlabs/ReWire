{-# LANGUAGE DataKinds #-}
-- module Tutorial.RISCV32i.BasicRV32I where
module BasicRV32I where

import Prelude hiding ((^), (+), (==), (&&))
import ReWire.Bits 
import ReWire

-- import ReWire.Interactive

-- | check out the README for the provenance of this file and project.

type W3  = W 3
type W4  = W 4
type W5  = W 5
type W6  = W 6
type W7  = W 7
type W8  = W 8
type W10 = W 10
type W12 = W 12
type W20 = W 20
type W32 = W 32

-- | See the Patterson/Waterman RISC-V text, Fig. 3.2., page 34.
data Register = R0
              | RA                          
              | SP                   
              | GP                         
              | TP                    
              | T0                         
              | T1 | T2                   
              | S0                  
              | S1                          
              | A0 | A1                     
              | A2 | A3 | A4 | A5 | A6 | A7 
              | S2 | S3 | S4 | S5 | S6 | S7 
              | S8 | S9 | S10 | S11
              | T3 | T4 | T5 | T6 
              | PC                          

data RegFile = RegFile { r0                          -- hardwired 0
                       , ra                          -- return address
                       , sp                          -- stack pointer
                       , gp                          -- global pointer
                       , tp                          -- thread pointer
                       , t0                          -- temp., alt. link reg.  
                       , t1 , t2                     -- temporaries
                       , s0                          -- aka, FP 
                       , s1                          -- saved register
                       , a0 , a1                     -- fun. args/return vals
                       , a2 , a3 , a4 , a5 , a6 , a7 -- fun args
                       , s2 , s3 , s4 , s5 , s6      -- saved registers
                       , s7 , s8 , s9 , s10 , s11    --   "      "
                       , t3 , t4 , t5 , t6           -- temporaries
                       , pc                          -- program counter (not in PW)
                         :: W32
                       }

rdReg :: Register -> RegFile -> W32
rdReg r = case r of
    R0 -> r0
    RA  -> ra 
    SP  -> sp 
    GP  -> gp 
    TP  -> tp 
    T0  -> t0
    T1  -> t1
    T2  -> t2
    S0  -> s0 
    S1  -> s1
    A0 -> a0
    A1 -> a1
    A2 -> a2
    A3 -> a3
    A4 -> a4
    A5 -> a5
    A6 -> a6
    A7 -> a7
    S2 -> s2
    S3 -> s3
    S4 -> s4
    S5 -> s5
    S6 -> s6
    S7 -> s7
    S8 -> s8
    S9 -> s9
    S10 -> s10
    S11 -> s11
    T3 -> t3
    T4 -> t4
    T5 -> t5
    T6 -> t6
    PC  -> pc 
    
wrReg :: Register -> W32 -> RegFile -> RegFile
wrReg r w rf = case r of
    R0  -> rf { r0 = lit 0 }
    RA  -> rf { ra = w }
    SP  -> rf { sp = w }
    GP  -> rf { gp = w }
    TP  -> rf { tp = w }
    T0  -> rf { t0 = w }
    T1  -> rf { t1 = w }
    T2  -> rf { t2 = w }
    S0  -> rf { s0 = w }
    S1  -> rf { s1 = w }
    A0  -> rf { a0 = w }
    A1  -> rf { a1 = w }
    A2  -> rf { a2 = w }
    A3  -> rf { a3 = w }
    A4  -> rf { a4 = w }
    A5  -> rf { a5 = w }
    A6  -> rf { a6 = w }
    A7  -> rf { a7 = w }
    S2  -> rf { s2 = w }
    S3  -> rf { s3 = w }
    S4  -> rf { s4 = w }
    S5  -> rf { s5 = w }
    S6  -> rf { s6 = w }
    S7  -> rf { s7 = w }
    S8  -> rf { s8 = w }
    S9  -> rf { s9 = w }
    S10 -> rf { s10 = w }
    S11 -> rf { s11 = w }
    T3  -> rf { t3 = w }
    T4  -> rf { t4 = w }
    T5  -> rf { t5 = w }
    T6  -> rf { t6 = w }
    PC  -> rf { pc = w }
    

getReg :: Monad m => Register -> StateT RegFile m W32
getReg r = do
  rs <- get
  return $ rdReg r rs

putReg :: Monad m => Register -> W32 -> StateT RegFile m ()
putReg r w = do
  rs <- get
  put $ wrReg r w rs

{-


signextW12_to_W32 :: W12 -> W32
signextW12_to_W32 (W12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
         = (W32 b11 C C C C C C C C C C C C C C C C C C C C b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)

signextW5_to_W32 :: W5 -> W32
signextW5_to_W32 (W5 b4 b3 b2 b1 b0)
        = (W32 b4 C C C C C C C C C C C C C C C C C C C C C C C C C C C b3 b2 b1 b0)
addW32 :: W32 -> W32 -> Bit -> (Bit, W32)
addW32 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) (W32 c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0) ci = (co , (fourw8tow32 d1 d2 d3 d4))
  where b1' = (W8 b31 b30 b29 b28 b27 b26 b25 b24)
        b2' = (W8 b23 b22 b21 b20 b19 b18 b17 b16)
        b3' = (W8 b15 b14 b13 b12 b11 b10 b9 b8)
        b4' = (W8 b7 b6 b5 b4 b3 b2 b1 b0)
        c1' = (W8 c31 c30 c29 c28 c27 c26 c25 c24)
        c2' = (W8 c23 c22 c21 c20 c19 c18 c17 c16)
        c3' = (W8 c15 c14 c13 c12 c11 c10 c9 c8)
        c4' = (W8 c7 c6 c5 c4 c3 c2 c1 c0)
        (co, d1)  = addW8 b1' c1' co1
        (co1, d2) = addW8 b2' c2' co2
        (co2, d3) = addW8 b3' c3' co3
        (co3, d4) = addW8 b4' c4' ci

addW8 :: W8 -> W8 -> Bit -> (Bit, W8)
addW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0, W8 d0 d1 d2 d3 d4 d5 d6 d7)
   where (co0,d0) = addBit b0 c0 co1
         (co1,d1) = addBit b1 c1 co2
         (co2,d2) = addBit b2 c2 co3
         (co3,d3) = addBit b3 c3 co4
         (co4,d4) = addBit b4 c4 co5
         (co5,d5) = addBit b5 c5 co6
         (co6,d6) = addBit b6 c6 co7
         (co7,d7) = addBit b7 c7 ci

fourw8tow32 :: W8 -> W8 -> W8 -> W8 -> W32
fourw8tow32 (W8 w1 w2 w3 w4 w5 w6 w7 w8) (W8 e1 e2 e3 e4 e5 e6 e7 e8) (W8 s1 s2 s3 s4 s5 s6 s7 s8) (W8 d1 d2 d3 d4 d5 d6 d7 d8) = (W32 w1 w2 w3 w4 w5 w6 w7 w8 e1 e2 e3 e4 e5 e6 e7 e8 s1 s2 s3 s4 s5 s6 s7 s8 d1 d2 d3 d4 d5 d6 d7 d8)

-- --
-- -- Accessor functions
-- --


-- --putRB :: Monad m => RegState -> StateT (RegState,InboundSignals,OutboundSignals) m ()
-- --getRB :: Monad m => StateT (RegState,InboundSignals,OutboundSignals) m RegState
-- putRB rb = do
--   (_,mi,mo) <- get
--   put (rb,mi,mo)
-- getRB = do
--   (regBank,_,_) <- get
--   return regBank

-- --getReg :: Monad m => Register -> RVRe m W32
-- getReg r = lift $ do
--   rb <- getRB
--   return $ rdReg r rb

-- --getReg' :: Monad m => Register -> StateT (RegState,InboundSignals,OutboundSignals) m W32
-- getReg' r = do
--   rb <- getRB
--   return $ rdReg r rb

-- --putReg :: Monad m => Register -> W32 -> RVRe m ()
-- putReg r w = lift $ do
--   rb <- getRB
--   putRB $ wrReg r w rb

--putPC :: Monad m => W32 -> StateT (RegState,InboundSignals,OutboundSignals) m ()
--putPC loc = do
--  rb <- getRB
--  putRB $ wrReg PC loc rb

--getPC :: Monad m => StateT (RegState,InboundSignals,OutboundSignals) m W32
--getPC = do
--  (regBank,_,_) <- get
--  let pc = rdReg PC regBank
--  return pc


-- Here, what I'm trying to do is square
--  1. what we did before,
--  2. the assembly language output of riscv64-unknown-elf-gcc, and
--  3. the Patterson/Waterman RISC-V text.
-- See PW, Fig. 3.2., page 34.


andW32 :: W32 -> W32 -> W32
andW32 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) (W32 c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0) = res
  where
    w1 = W8 b31 b30 b29 b28 b27 b26 b25 b24
    w2 = W8 b23 b22 b21 b20 b19 b18 b17 b16
    w3 = W8 b15 b14 b13 b12 b11 b10 b9 b8
    w4 = W8 b7 b6 b5 b4 b3 b2 b1 b0
    w5 = W8 c31 c30 c29 c28 c27 c26 c25 c24
    w6 = W8 c23 c22 c21 c20 c19 c18 c17 c16
    w7 = W8 c15 c14 c13 c12 c11 c10 c9 c8
    w8 = W8 c7 c6 c5 c4 c3 c2 c1 c0
    res = fourw8tow32 (andW8 w1 w5) (andW8 w2 w6) (andW8 w3 w7) (andW8 w4 w8)

andW8 :: W8 -> W8 -> W8
andW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (andBit b0 c0) (andBit b1 c1)
                                                                     (andBit b2 c2) (andBit b3 c3)
                                                                     (andBit b4 c4) (andBit b5 c5)
                                                                     (andBit b6 c6) (andBit b7 c7)

andBit :: Bit -> Bit -> Bit
andBit S b = b
andBit C _ = C

sLessThan :: W32 -> W32 -> W32
sLessThan w1 w2
  | bit2bool $ isPositive w1 =
    if bit2bool $ isNegative w2
      then zero
      else sLessThanHelperPos w1 w2
  | otherwise =
    if bit2bool $ isPositive w2
      then one
      else sLessThanHelperNeg w1 w2

sLessThanHelperPos :: W32 -> W32 -> W32
sLessThanHelperPos w1 w2
  | bit2bool $ isNegative $ (snd $ subW32 w1 w2 C) = zero
  | otherwise                 = one

sLessThanHelperNeg :: W32 -> W32 -> W32
sLessThanHelperNeg w1 w2
  | bit2bool $ isPositive $ (snd $ subW32 w1 w2 C) = zero
  | otherwise                 = one

isNegative :: W32 -> Bit
isNegative (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) =
  if bit2bool $ biteq b31 S
    then S
    else C

-- Bit Functions --

notBit :: Bit -> Bit
notBit S = C
notBit C = S

bit2bool :: Bit -> Bool
bit2bool S = True
bit2bool C = False

bool2bit :: Bool -> Bit
bool2bit True = S
bool2bit False = C

biteq :: Bit -> Bit -> Bit
biteq S S = S
biteq C C = S
biteq _ _ = C

bToInteger :: Bit -> Integer
bToInteger C = 0
bToInteger S = 1

w5ToInteger :: W5 -> Integer
w5ToInteger (W5 b0 b1 b2 b3 b4) = foldr (+) 0 [b4',b3',b2',b1',b0']
  where b4' = (bToInteger b4)
        b3' = (bToInteger b3) * 2 ^ 1
        b2' = (bToInteger b2) * 2 ^ 2
        b1' = (bToInteger b1) * 2 ^ 3
        b0' = (bToInteger b0) * 2 ^ 4

addBit :: Bit -> Bit -> Bit -> (Bit, Bit)
addBit C C C = (C,C)
addBit C C S = (C,S)
addBit C S C = (C,S)
addBit C S S = (S,C)
addBit S C C = (C,S)
addBit S C S = (S,C)
addBit S S C = (S,C)
addBit S S S = (S,S)

subBit :: Bit -> Bit -> Bit -> (Bit, Bit)
subBit C C C = (C,C)
subBit C C S = (S,S)
subBit C S C = (S,S)
subBit C S S = (S,C)
subBit S C C = (C,S)
subBit S C S = (C,C)
subBit S S C = (C,C)
subBit S S S = (S,C)

zero :: W32
zero = (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C)

one :: W32
one  = (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S)

subW32 :: W32 -> W32 -> Bit -> (Bit, W32)
subW32 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) (W32 c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0) ci = (co , (fourw8tow32 d1 d2 d3 d4))
  where b1' = (W8 b31 b30 b29 b28 b27 b26 b25 b24)
        b2' = (W8 b23 b22 b21 b20 b19 b18 b17 b16)
        b3' = (W8 b15 b14 b13 b12 b11 b10 b9 b8)
        b4' = (W8 b7 b6 b5 b4 b3 b2 b1 b0)
        c1' = (W8 c31 c30 c29 c28 c27 c26 c25 c24)
        c2' = (W8 c23 c22 c21 c20 c19 c18 c17 c16)
        c3' = (W8 c15 c14 c13 c12 c11 c10 c9 c8)
        c4' = (W8 c7 c6 c5 c4 c3 c2 c1 c0)
        (co, d1)  = subW8 b1' c1' co1
        (co1, d2) = subW8 b2' c2' co2
        (co2, d3) = subW8 b3' c3' co3
        (co3, d4) = subW8 b4' c4' ci

subW8 :: W8 -> W8 -> Bit -> (Bit, W8)
subW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0 ,W8 d0 d1 d2 d3 d4 d5 d6 d7)
   where (co0,d0) = subBit b0 c0 co1
         (co1,d1) = subBit b1 c1 co2
         (co2,d2) = subBit b2 c2 co3
         (co3,d3) = subBit b3 c3 co4
         (co4,d4) = subBit b4 c4 co5
         (co5,d5) = subBit b5 c5 co6
         (co6,d6) = subBit b6 c6 co7
         (co7,d7) = subBit b7 c7 ci

isPositive :: W32 -> Bit
isPositive (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C) = C
isPositive w32                                                                   = notBit $ isNegative w32

uLessThan :: W32 -> W32 -> W32
uLessThan w1 w2
  | bit2bool $ isPositive $ (snd $ subW32 w1 w2 C) = zero
  | otherwise                                      = one

uLessThanEqual :: W32 -> W32 -> W32
uLessThanEqual w1 w2
  | bit2bool $ isZero dif      = one
  | bit2bool $ isNegative dif  = one
  | otherwise                  = zero
    where dif = snd $ subW32 w1 w2 C

uGreaterThan :: W32 -> W32 -> W32
uGreaterThan w1 w2
  | bit2bool $ isPositive $ (snd $ subW32 w1 w2 C) = one
  | otherwise                                      = zero

isZero :: W32 -> Bit
isZero (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C)
         = S
isZero _ = C

logShiftLW32 :: (Bit, W32) -> Integer -> (Bit, W32)
logShiftLW32 shift@(b, W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) x =
   if x > 0
   then let b1' = (W8 b31 b30 b29 b28 b27 b26 b25 b24)
            b2' = (W8 b23 b22 b21 b20 b19 b18 b17 b16)
            b3' = (W8 b15 b14 b13 b12 b11 b10 b9 b8)
            b4' = (W8 b7 b6 b5 b4 b3 b2 b1 b0)
            (co, d1)  = arthShiftLW8 b1' co1
            (co1, d2) = arthShiftLW8 b2' co2
            (co2, d3) = arthShiftLW8 b3' co3
            (co3, d4) = arthShiftLW8 b4' C
        in logShiftLW32 (co, (fourw8tow32 d1 d2 d3 d4)) (x - 1)
   else shift

arthShiftLW8 :: W8 -> Bit -> (Bit, W8)
arthShiftLW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b0, W8 b1 b2 b3 b4 b5 b6 b7 ci)

xorW32 :: W32 -> W32 -> W32
xorW32 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) (W32 c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0) = res
  where
    w1 = W8 b31 b30 b29 b28 b27 b26 b25 b24
    w2 = W8 b23 b22 b21 b20 b19 b18 b17 b16
    w3 = W8 b15 b14 b13 b12 b11 b10 b9 b8
    w4 = W8 b7 b6 b5 b4 b3 b2 b1 b0
    w5 = W8 c31 c30 c29 c28 c27 c26 c25 c24
    w6 = W8 c23 c22 c21 c20 c19 c18 c17 c16
    w7 = W8 c15 c14 c13 c12 c11 c10 c9 c8
    w8 = W8 c7 c6 c5 c4 c3 c2 c1 c0
    res = fourw8tow32 (xorW8 w1 w5) (xorW8 w2 w6) (xorW8 w3 w7) (xorW8 w4 w8)

xorW8 :: W8 -> W8 -> W8
xorW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8  (xorBit b0 c0) (xorBit b1 c1)
                                                                      (xorBit b2 c2) (xorBit b3 c3)
                                                                      (xorBit b4 c4) (xorBit b5 c5)
                                                                      (xorBit b6 c6) (xorBit b7 c7)

xorBit :: Bit -> Bit -> Bit
xorBit S b = notBit b
xorBit C b = b

orW32 :: W32 -> W32 -> W32
orW32 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) (W32 c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0) = res
  where
    w1 = W8 b31 b30 b29 b28 b27 b26 b25 b24
    w2 = W8 b23 b22 b21 b20 b19 b18 b17 b16
    w3 = W8 b15 b14 b13 b12 b11 b10 b9 b8
    w4 = W8 b7 b6 b5 b4 b3 b2 b1 b0
    w5 = W8 c31 c30 c29 c28 c27 c26 c25 c24
    w6 = W8 c23 c22 c21 c20 c19 c18 c17 c16
    w7 = W8 c15 c14 c13 c12 c11 c10 c9 c8
    w8 = W8 c7 c6 c5 c4 c3 c2 c1 c0
    res = fourw8tow32 (orW8 w1 w5) (orW8 w2 w6) (orW8 w3 w7) (orW8 w4 w8)

orW8 :: W8 -> W8 -> W8
orW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (orBit b0 c0) (orBit b1 c1)
                                                                    (orBit b2 c2) (orBit b3 c3)
                                                                    (orBit b4 c4) (orBit b5 c5)
                                                                    (orBit b6 c6) (orBit b7 c7)
orBit :: Bit -> Bit -> Bit
orBit C b = b
orBit S _ = S

logShiftRW32 :: (Bit, W32) -> Integer -> (Bit, W32)
logShiftRW32 shift@(b, W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) x =
   if x > 0
   then let b1' = (W8 b31 b30 b29 b28 b27 b26 b25 b24)
            b2' = (W8 b23 b22 b21 b20 b19 b18 b17 b16)
            b3' = (W8 b15 b14 b13 b12 b11 b10 b9 b8)
            b4' = (W8 b7 b6 b5 b4 b3 b2 b1 b0)
            (co, d1)  = arthShiftRW8 b1' C
            (co1, d2) = arthShiftRW8 b2' co
            (co2, d3) = arthShiftRW8 b3' co1
            (co3, d4) = arthShiftRW8 b4' co2
        in logShiftRW32 (co3, (fourw8tow32 d1 d2 d3 d4)) (x - 1)
   else shift

arthShiftRW8 :: W8 -> Bit -> (Bit, W8)
arthShiftRW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b7, W8 ci b0 b1 b2 b3 b4 b5 b6)


arthShiftRW32 :: (Bit, W32) -> Integer -> (Bit, W32)
arthShiftRW32 shift@(b, W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) x =
    if x > 0
    then let b1' = (W8 b31 b30 b29 b28 b27 b26 b25 b24)
             b2' = (W8 b23 b22 b21 b20 b19 b18 b17 b16)
             b3' = (W8 b15 b14 b13 b12 b11 b10 b9 b8)
             b4' = (W8 b7 b6 b5 b4 b3 b2 b1 b0)
             (co, d1)  = arthShiftRW8 b1' b0
             (co1, d2) = arthShiftRW8 b2' co
             (co2, d3) = arthShiftRW8 b3' co1
             (co3, d4) = arthShiftRW8 b4' co2
        in arthShiftRW32 (co3, (fourw8tow32 d1 d2 d3 d4)) (x - 1)
    else shift

lui :: W20 -> W32
lui (W20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = (W32 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0 C C C C C C C C C C C C)

--converts int to a list of 1's and 0's
intConv :: Int -> [Int]
intConv w = intConv' [] w
  where
    intConv' :: [Int] -> Int -> [Int]
    intConv' a 0 = reverse a
    intConv' a x = intConv' (a ++ [(rem x 2)]) (quot x 2)

intsToBits :: [Int] -> [Bit]
intsToBits a = map (c) (bits' (32 - length a) a)
  where
    bits' 0 a = a
    bits' x a = bits' (x-1) (0:a)
    c 0 = C
    c 1 = S

listToW32 :: [Bit] -> W32
listToW32 (b31:b30:b29:b28:b27:b26:b25:b24:b23:b22:b21:b20:b19:b18:b17:b16:b15:b14:b13:b12:b11:b10:b9:b8:b7:b6:b5:b4:b3:b2:b1:b0:[]) =
  W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0

listToW20 :: [Bit] -> W20
listToW20 (b19:b18:b17:b16:b15:b14:b13:b12:b11:b10:b9:b8:b7:b6:b5:b4:b3:b2:b1:b0:[]) = W20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0

listToW5 :: [Bit] -> W5
listToW5 (b4:b3:b2:b1:b0:_) = W5 b4 b3 b2 b1 b0

listToW12 :: [Bit] -> W12
listToW12 (b11:b10:b9:b8:b7:b6:b5:b4:b3:b2:b1:b0:[]) = W12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0
-}
