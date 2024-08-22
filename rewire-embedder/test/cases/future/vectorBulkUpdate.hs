{-# LANGUAGE DataKinds #-}
import Prelude hiding (replicate, map, take, iterate, zip, (*))
import ReWire ( signal, Identity, ReacT, Vec, W, Proxy (..))
import ReWire.Monad (Dev, iter)
import ReWire.Finite ( finite )
import ReWire.FiniteComp ( (*) )

import ReWire.Vectors (take, map, replicate, iterate, bulkUpdate, zip)
import ReWire.Bits (lit)

type Input = (Vec 8 (W 8), Vec 4 (W 8))
type Output = Vec 8 (W 8)

start :: Dev Input Output
start = iter compute (initVec, take initVec)

initVec :: Vec 8 (W 8)
initVec = replicate (lit 0)


-- | Vectors
-- `bulkUpdate, `zip, `iterate,
compute :: Input -> Output
compute (v,w) = bulkUpdate v (zip (iterate (Proxy :: Proxy 4) (* 2) (finite 0)) w)

main = undefined
