{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Monad (Dev, iter)
import ReWire.Bits (lit, (.&.), (.|.), bnot, (^), (~^))

start :: Dev (W 8,W 8,W 8) (W 8)
start = iter compute (lit 0,lit 1,lit 2)

-- | Bits
-- (And, ".&."), (Or, ".|."), (Not, "bnot"), (XOr, "^"), (XNor, "~^")
compute :: (W 8,W 8,W 8) -> W 8
compute (u,v,w) = (u .&. v ^ w) .|. (bnot u .&. bnot v ~^ w)

main = undefined