{-# LANGUAGE DataKinds #-}
import Prelude hiding (replicate, map, take, drop, (++), reverse, zipWith)
import ReWire (signal, Identity, ReacT, Vec, W, Finite, Proxy(..))
import ReWire.Monad (Dev, iter)
import ReWire.Vectors (zipWith, index, lastIndex, map, replicate, slice) 
import ReWire.Bits (lit, (!%))
import ReWire.Finite (finite)

type Input = (Vec 8 (W 8), Vec 16 (Vec 4 (Finite 8)))
type Output = Vec 8 Bool

start :: Dev Input Output
start = iter compute (initVec, replicate (replicate (finite 0)))

initVec :: Vec 8 (W 8)
initVec = map lit (replicate 0 :: Vec 8 Integer)


-- | Vectors
-- `index, lastIndex, `slice, zipWith
compute :: Input -> Output
compute (v,w) = zipWith (\ v' w' -> v' !% (index w' (lastIndex w'))) v (slice (Proxy :: Proxy 4) w)

main = undefined
