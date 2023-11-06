{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Prelude hiding ((+), (*), head, last, tail, map, replicate, Word)
import ReWire
import ReWire.Bits
import ReWire.Vectors

type Word = W 100
type Packed = (Word, Word, Word, Word)

x2 :: Word -> Word
x2 x = x * lit 2

dev :: Word -> ReacT Word Packed Identity ()
dev a = do
      let b = x2 a
      a'  <- signal (unpacklo a b, unpackhi a b, packlo a b, packhi a b)
      dev a'

start :: ReacT Word Packed Identity ()
start = dev $ lit 0

main :: IO ()
main = undefined
