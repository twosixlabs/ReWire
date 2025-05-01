import ReWire
import ReWire.Bits
import ReWire.Monad

data SRec = SRec { f1, f2 :: Bit, f3 :: (Bit, Bit) }

x :: SRec -> Bit
x d = f1 d

y :: SRec -> Bit
y d = f2 d

z :: SRec -> (Bit, Bit)
z d = f3 d

foo :: SRec -> SRec
foo (SRec { f1 = a, f2 = b, f3 = c }) = SRec { f1 = b, f2 = a, f3 = c }

bar :: SRec -> SRec
bar s@(SRec {}) = s { f1 = False, f2 = True, f3 = (False, True) }

compute :: Bit -> Bit
compute _ = x $ foo $ bar (SRec False False (False, False))

start :: Dev Bit Bit
start = iter compute False

main = undefined
