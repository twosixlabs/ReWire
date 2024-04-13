{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^), (+), (==), (&&))
import ReWire.Bits
import ReWire

import ReWire.Interactive

gennats8 :: W8 -> [W8]
gennats8 w = w : gennats8 (w + lit 1)

nats8 :: [W8]
nats8 = gennats8 (lit 0)

---

type W8    = W 8

type Device = ReacT W8 W8 (StateT W8 Identity) ()

accum :: W8 -> StateT W8 Identity W8
accum i = do
           acc <- get
           let v = i + acc
           put v
           return v

loop :: W8 -> Device
loop i = lift (accum i) >>= signal >>= loop

start :: ReacT W8 W8 Identity ()
start = extrude (loop (lit 0)) (lit 0)
