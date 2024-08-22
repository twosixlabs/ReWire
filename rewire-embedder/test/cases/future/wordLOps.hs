{-# LANGUAGE DataKinds #-}
import Prelude hiding ((||),(&&))
import ReWire (signal, Identity, ReacT, W)
import ReWire.Monad (Dev, iter)
import ReWire.Bits (lit,(&&.),(||.),lnot,(&&),(||))


start :: Dev (W 8, W 8) Bool
start = iter compute (lit 0, lit 1)

-- | Bits
--   (LAnd, "&&."), (LOr, "||."), (LNot, "lnot"),
compute :: (W 8,W 8) -> Bool
compute (v,w) = (v &&. w) || ((v ||. w) && lnot w)

main = undefined