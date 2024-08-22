{-# LANGUAGE DataKinds #-}
import Prelude hiding (replicate, map, zipWith, id, ($), (>>=), zip)
import ReWire ( signal, Identity, ReacT, Vec, W, Bit)
import ReWire.Prelude (id, ($), (>>=))
import ReWire.Monad (Dev, iter)
import ReWire.Vectors ((!=), index, generate, zipWith, map, replicate, zip)
import ReWire.Bits (lit)

type Input = (Vec 8 (W 8), Vec 8 Bit)
type Output = Vec 8 (W 8)

start :: Dev Input Output
start = iter compute (initVec, lit 213)

initVec :: Vec 8 (W 8)
initVec = map lit (replicate 0 :: Vec 8 Integer)


-- | Vectors
-- `update/(!=), zipWith3, `generate
compute :: Input -> Output
compute (v,w) = zipWith (uncurry (!=)) (zip v (generate id)) (generate $ index w)

main = undefined
