{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire ( signal, Identity, ReacT, W, Finite)
import ReWire.Monad (Dev, iter)
import ReWire.Bits (lit)
import ReWire.Finite (finite,toFinite,toFinite',fromFinite)
import ReWire.FiniteComp ((+))

start :: Dev (Finite 128) (Finite 20)
start = iter compute (finite 77)

-- | Finite
-- finite : Integer -> Finite n, toFinite : W m -> Finite n
-- toFinite' : W m -> Finite n [uses mod instead of error]
-- fromFinite : Finite n -> W m
compute :: Finite 128 -> Finite 20
compute n = toFinite' (fromFinite n :: W 7) + toFinite (lit 6 :: W 3)

main = undefined