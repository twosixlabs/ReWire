{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, left identity" #-}
import Prelude hiding ((>),(==),(<),(>=),(<=))
import ReWire (signal, Identity, ReacT, W)
import ReWire.Monad (Dev, iter)
import ReWire.Bits ((>),(==),(<),(>=),(<=),lit)


start :: Dev (W 8, W 8) Bool
start = iter compute (lit 0, lit 1)

-- | Bits
-- (Eq, "=="), (Gt, ">"), (GtEq, ">="), (Lt, "<"), (LtEq, "<=")
compute :: (W 8,W 8) -> Bool
compute (v,w) = ((v < w) && not (v >= w)) || (v == w) || ((v > w) && not (v <= w))

main = undefined
