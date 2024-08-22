{-# LANGUAGE DataKinds #-}
import Prelude hiding (replicate, map, take, zipWith, (+), (++))
import ReWire (signal, Identity, ReacT, Vec, W, Proxy(..))
import ReWire.Monad (Dev, iter)
import ReWire.Vectors (map, (!), replicate, rslice)
import ReWire.Bits (lit, (+))

type Input = (Vec 16 (Vec 8 (W 8)), Vec 4 (W 8))
type Output = Vec 8 (W 8)

start :: Dev Input Output
start = iter loop (replicate (replicate (lit 0)), replicate (lit 0))

loop :: Input -> Output
loop (v,w) = compute v w

-- | Vectors
-- `index'/(!), lastIndex', `rslice, zipWith
compute :: Vec 16 (Vec 8 (W 8)) -> Vec 4 (W 8) -> Output
compute v w = map (\ v' -> (w ! (Proxy :: Proxy 3)) + (v' ! (Proxy :: Proxy 3))) 
                       (rslice (Proxy :: Proxy 8) v)

main = undefined
