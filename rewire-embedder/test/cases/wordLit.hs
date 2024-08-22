{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Bits ( lit, resize, (^)) 
import ReWire.Monad (Dev, iter)


start :: Dev (W 8) (W 16)
start = iter compute (lit 255)

compute :: W 8 -> W 16
compute i = (resize i) ^ (lit 1)

main = undefined

-- | Bits
{-
      (Resize, "resize"),
      also lit, 
-}