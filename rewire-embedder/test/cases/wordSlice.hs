{-# LANGUAGE DataKinds #-}
import ReWire ( signal, Identity, ReacT, W )
import ReWire.Monad (Dev, iter)
import ReWire.Bits (lit, (@@), (@.), msbit)

start :: Dev (W 16) (W 8)
start = iter compute (lit 0)

-- | Bits
-- (BitSlice, "finBitSlice"),(BitIndex, "finBitIndex"), (MSBit, "msbit")
compute :: W 16 -> W 8
compute w = if msbit w && w @. 8 
            then w @@ (7,0)
            else w @@ (15,8)

main = undefined