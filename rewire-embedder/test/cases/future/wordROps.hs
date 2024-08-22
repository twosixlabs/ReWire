{-# LANGUAGE DataKinds #-}
import Prelude hiding ((&&),(||))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Monad (Dev, iter)
import ReWire.Bits (lit, rAnd, rNAnd, rOr, rNor, rXOr,rXNor,(||),(&&))

start :: Dev (W 8,W 8,W 8) Bool
start = iter compute (lit 0,lit 1,lit 2)

-- | Bits
-- (RAnd, "rAnd"), (RNAnd, "rNAnd"), (ROr, "rOr"), (RNor, "rNor"), (RXOr, "rXOr"), (RXNor, "rXNor")
compute :: (W 8,W 8,W 8) -> Bool
compute (u,v,w) = (rAnd u && rOr v && rXOr w) || (rNAnd u && rNor v && rXNor w)

main = undefined
