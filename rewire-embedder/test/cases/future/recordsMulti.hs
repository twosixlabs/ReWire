import ReWire
import ReWire.Bits

data SRec = S1 { f1, f2 :: Bit, f3 :: (Bit, Bit) } 
          | S2 { f4 :: (Bit, Bit, Bit) }

x :: SRec -> Bit
x d = f1 d

y :: SRec -> Bit
y d = f2 d

z :: SRec -> (Bit, Bit)
z d = f3 d

w :: SRec -> (Bit, Bit, Bit)
w d = f4 d

foo :: SRec -> SRec
foo (S1 { f1 = a, f2 = b, f3 = c }) = S1 { f1 = b, f2 = a, f3 = c }
foo (S2 { f4 = a }) = S2 { f4 = a }

bar :: SRec -> SRec
bar s@(S1 {}) = s { f1 = zero, f2 = one, f3 = (zero, one) }
bar s@(S2 {}) = s { f4 = (zero, one, zero) }

start :: ReacT Bit Bit Identity ()
start = do
  signal $ x $ foo $ bar (S1 zero zero (zero, zero))
  start

main = undefined
