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

incr :: W8 -> W8
incr w = w + lit 1

inc :: W8 -> Device
inc i = signal (incr i) >>= inc

start :: ReacT W8 W8 Identity ()
start = extrude (inc (lit 0)) (lit 99)
