{-# LANGUAGE DataKinds #-}
import Prelude hiding (replicate, map, take, drop, (++), reverse)
import ReWire ( signal, Identity, ReacT, Vec, W)
import ReWire.Monad (Dev, iter)
import ReWire.Vectors (reverse, (++), empty, singleton, take, drop, map, replicate)
import ReWire.Bits (lit)

type Input = (Vec 8 (W 8), Vec 8 (W 8))
type Output = Vec 8 (W 8)

start :: Dev Input Output
start = iter compute (initVec, initVec)

initVec :: Vec 8 (W 8)
initVec = map lit (replicate 0 :: Vec 8 Integer)


-- | Vectors
-- reverse, `(++), empty, singleton, take, drop
compute :: Input -> Output
compute (v,w) = ((((reverse (take v :: Vec 4 (W 8)) 
             ++ empty) :: Vec 4 (W 8))
             ++ singleton (lit 0)) :: Vec 5 (W 8))
             ++ (drop w :: Vec 3 (W 8))

-- | Note that when using vector append, (++), all resultant computations
--   need type annotations (incluing intermediate results).


main = undefined


