module ComputerArithmetic where

{-
This module defines word types and adds them to Num, using two's complement representation
and standard definitions for binary arithmetic (esp., Booth's algorithm for multiplication).
-}

data Bit = C | S deriving (Eq,Read)
instance Show Bit where
  show C = "0"
  show S = "1"
data W4 = W4 Bit Bit Bit Bit deriving Eq
data W5 = W5 Bit Bit Bit Bit Bit deriving Eq
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving (Eq,Read)
data W9 = W9 Bit Bit Bit Bit Bit Bit Bit Bit Bit deriving Eq

w4Tow8 :: W4 -> W8
w4Tow8 (W4 b3 b2 b1 b0) = W8 C C C C b3 b2 b1 b0

instance Show W4 where
  show (W4 b3 b2 b1 b0) = "0b" ++ show b3 ++ show b2 ++ show b1 ++ show b0

instance Show W5 where
  show (W5 b4 b3 b2 b1 b0) = "0b" ++ show b4 ++ show b3 ++ show b2 ++ show b1 ++ show b0

instance Show W8 where
  show (W8 b7 b6 b5 b4 b3 b2 b1 b0) = "0b" ++ show b7 ++ show b6 ++ show b5 ++ show b4 ++ show b3 ++ show b2 ++ show b1 ++ show b0

instance Show W9 where
  show (W9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = "0b" ++ show b8 ++ show b7 ++ show b6 ++ show b5 ++ show b4 ++ show b3 ++ show b2 ++ show b1 ++ show b0

instance Num W4 where
  (+)         = (<+>)
  (*)         = (ComputerArithmetic.<*>)
  abs w4      = case signbit w4 of
    C -> w4
    S -> ComputerArithmetic.negate w4
  signum w4   = case signbit w4 of
    C -> one
    S -> ComputerArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bitwiseneg i') <+> one
    where
      [b2,b1,b0] = pad_or_trunc 3 i
      i'         = W4 C b2 b1 b0
  negate      = ComputerArithmetic.negate

instance Num W5 where
  (+)         = (<+>)
  (*)         = (ComputerArithmetic.<*>)
  abs w5      = case signbit w5 of
    C -> w5
    S -> ComputerArithmetic.negate w5
  signum w5   = case signbit w5 of
    C -> one
    S -> ComputerArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bitwiseneg i') <+> one
    where
      [b3,b2,b1,b0] = pad_or_trunc 4 i
      i'            = W5 C b3 b2 b1 b0
  negate      = ComputerArithmetic.negate

instance Num W8 where
  (+)         = (<+>)
  (*)         = (ComputerArithmetic.<*>)
  abs w8      = case signbit w8 of
    C -> w8
    S -> ComputerArithmetic.negate w8
  signum w8   = case signbit w8 of
    C -> one
    S -> ComputerArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bitwiseneg i') <+> one
    where
      [b6,b5,b4,b3,b2,b1,b0] = pad_or_trunc 7 i
      i'                     = W8 C b6 b5 b4 b3 b2 b1 b0
  negate      = ComputerArithmetic.negate

instance Num W9 where
  (+)         = (<+>)
  (*)         = (ComputerArithmetic.<*>)
  abs w9      = case signbit w9 of
    C -> w9
    S -> ComputerArithmetic.negate w9
  signum w9   = case signbit w9 of
    C -> one
    S -> ComputerArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bitwiseneg i') <+> one
    where
      [b7,b6,b5,b4,b3,b2,b1,b0] = pad_or_trunc 8 i
      i'                     = W9 C b7 b6 b5 b4 b3 b2 b1 b0
  negate      = ComputerArithmetic.negate


-- watch for fenceposts! 
pad_or_trunc :: Int -> Integer -> [Bit]
pad_or_trunc d i | l==d = revbits
                 | l<d  = pad (d-l) revbits
                 | l>d  = reverse $ take d bits
  where
    bits    = int2bin i
    l       = length bits
    revbits = reverse bits
    pad :: Int -> [Bit] -> [Bit]
    pad n bits | n==0      = bits
               | n>0       = C : pad (n-1) bits
               | otherwise = error "can't happen"
    
int2bin :: Integer -> [Bit]
int2bin i | i==0      = []
          | otherwise = b : int2bin (i `div` 2)
              where b = case i `mod` 2 of
                      0 -> C
                      1 -> S
                      _ -> error "can't happen"


class ComputerArith w where
   (<+>)      :: w -> w -> w
   leastbit   :: w -> Bit
   bitwiseneg :: w -> w
   rshift     :: (w,w) -> (w,w,Bit)
   zero, one  :: w
   booth      :: (w,w) -> (w,w)
   signbit    :: w -> Bit

instance ComputerArith W4 where
  (W4 a3 a2 a1 a0) <+> (W4 b3 b2 b1 b0) = W4 c3 c2 c1 c0
    where
      (carry0,c0) = bitplus a0 b0 C
      (carry1,c1) = bitplus a1 b1 carry0
      (carry2,c2) = bitplus a2 b2 carry1
      (_,c3)      = bitplus a3 b3 carry2
  leastbit (W4 _ _ _ b)       = b
  bitwiseneg (W4 a3 a2 a1 a0) = W4 (fBit a3) (fBit a2) (fBit a1) (fBit a0)
  rshift (W4 a3 a2 a1 a0, W4 q3 q2 q1 q0)
                      = (W4 a3 a3 a2 a1, W4 a0 q3 q2 q1, q0)
  zero                        = W4 C C C C
  one                         = W4 C C C S
  booth (w1,w2) = booth4 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth4         = proj . boothround . boothround . boothround . boothround
  signbit (W4 b3 _ _ _)    = b3
  
instance ComputerArith W5 where
  (W5 a4 a3 a2 a1 a0) <+> (W5 b4 b3 b2 b1 b0) = W5 c4 c3 c2 c1 c0
    where
      (carry0,c0) = bitplus a0 b0 C
      (carry1,c1) = bitplus a1 b1 carry0
      (carry2,c2) = bitplus a2 b2 carry1
      (carry3,c3) = bitplus a3 b3 carry2
      (_,c4)      = bitplus a4 b4 carry3
  leastbit (W5 _ _ _ _ b)     = b
  bitwiseneg (W5 a4 a3 a2 a1 a0) = W5 (fBit a4) (fBit a3) (fBit a2) (fBit a1) (fBit a0)
  rshift (W5 a4 a3 a2 a1 a0, W5 q4 q3 q2 q1 q0) = (W5 a4 a4 a3 a2 a1, W5 a0 q4 q3 q2 q1, q0)
  zero                        = W5 C C C C C
  one                         = W5 C C C C S
  booth (w1,w2) = booth5 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth5         = proj .
                               boothround .
                               boothround .
                               boothround .
                               boothround . boothround
  signbit (W5 b4 _ _ _ _)  = b4

instance ComputerArith W8 where
  (W8 a7 a6 a5 a4 a3 a2 a1 a0) <+> (W8 b7 b6 b5 b4 b3 b2 b1 b0) = W8 c7 c6 c5 c4 c3 c2 c1 c0
    where
      (carry0,c0) = bitplus a0 b0 C
      (carry1,c1) = bitplus a1 b1 carry0
      (carry2,c2) = bitplus a2 b2 carry1
      (carry3,c3) = bitplus a3 b3 carry2
      (carry4,c4) = bitplus a4 b4 carry3
      (carry5,c5) = bitplus a5 b5 carry4
      (carry6,c6) = bitplus a6 b6 carry5
      (_,c7)      = bitplus a7 b7 carry6
  leastbit (W8 _ _ _ _ _ _ _ b)  = b
  bitwiseneg (W8 a7 a6 a5 a4 a3 a2 a1 a0)
        = W8 (fBit a7) (fBit a6) (fBit a5) (fBit a4)
             (fBit a3) (fBit a2) (fBit a1) (fBit a0)
  rshift (W8 a7 a6 a5 a4 a3 a2 a1 a0, W8 q7 q6 q5 q4 q3 q2 q1 q0)
        = (W8 a7 a7 a6 a5 a4 a3 a2 a1, W8 a0 q7 q6 q5 q4 q3 q2 q1, q0)
  zero                        = W8 C C C C C C C C
  one                         = W8 C C C C C C C S
  booth (w1,w2) = booth8 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth8         = proj .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround . boothround
  signbit (W8 b7 _ _ _  _ _ _ _) = b7

instance ComputerArith W9 where
  (W9 a8 a7 a6 a5 a4 a3 a2 a1 a0) <+> (W9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = W9 c8 c7 c6 c5 c4 c3 c2 c1 c0
    where
      (carry0,c0) = bitplus a0 b0 C
      (carry1,c1) = bitplus a1 b1 carry0
      (carry2,c2) = bitplus a2 b2 carry1
      (carry3,c3) = bitplus a3 b3 carry2
      (carry4,c4) = bitplus a4 b4 carry3
      (carry5,c5) = bitplus a5 b5 carry4
      (carry6,c6) = bitplus a6 b6 carry5
      (carry7,c7) = bitplus a7 b7 carry6
      (_,c8)      = bitplus a8 b8 carry7
  leastbit (W9 _ _ _ _ _ _ _ _ b)  = b
  bitwiseneg (W9 a8 a7 a6 a5 a4 a3 a2 a1 a0)
        = W9 (fBit a8) (fBit a7) (fBit a6) (fBit a5) (fBit a4)
             (fBit a3) (fBit a2) (fBit a1) (fBit a0)
  rshift (W9 a8 a7 a6 a5 a4 a3 a2 a1 a0, W9 q8 q7 q6 q5 q4 q3 q2 q1 q0)
        = (W9 a8 a8 a7 a6 a5 a4 a3 a2 a1, W9 a0 q8 q7 q6 q5 q4 q3 q2 q1, q0)
  zero                        = W9 C C C C C C C C C
  one                         = W9 C C C C C C C C S
  booth (w1,w2) = booth9 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth9         = proj .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround . boothround
  signbit (W9 b8 _ _ _ _  _ _ _ _) = b8


-- flips the Bit.
fBit :: Bit -> Bit
fBit C = S
fBit S = C

bitplus :: Bit -> Bit -> Bit -> (Bit,Bit)
bitplus C C C = (C,C)
bitplus C C S = (C,S)
bitplus C S C = (C,S)
bitplus C S S = (S,C)
bitplus S C C = (C,S)
bitplus S C S = (S,C)
bitplus S S C = (S,C)
bitplus S S S = (S,S)

negate :: ComputerArith w => w -> w
negate w = (bitwiseneg w) <+> one

(<->) :: ComputerArith w => w -> w -> w
w1 <-> w2 = w1 <+> (ComputerArithmetic.negate w2)

(<*>) :: ComputerArith w => w -> w -> w
w1 <*> w2 = snd (booth (w1,w2))

boothround :: ComputerArith t => (t, t, Bit, t) -> (t, t, Bit, t)
boothround (a,q,q_1,m) = let
    q_0 = leastbit q in
    case (q_0,q_1) of
      (C,C) -> (a',q',q_1',m)
        where
          (a',q',q_1') = rshift (a,q)
        
      (S,C) -> (a'',q',q_1',m)           -- A - M
        where
          a'            = a <-> m
          (a'',q',q_1') = rshift (a',q)

      (C,S) -> (a'', q',q_1',m)     -- A + M
        where
          a'            = a <+> m
          (a'',q',q_1') = rshift (a',q)

      (S,S) -> (a',q',q_1',m)
        where
          (a',q',q_1')  = rshift (a,q)
