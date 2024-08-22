{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
import Prelude hiding (not, return, (>>=), (>>))
import ReWire ( signal, Identity, ReacT, StateT, Bit, extrude, lift, get, put)
import ReWire.Prelude ((>>=~), not, return, (>>=), (>>))
import ReWire.Bits ( xor )
import ReWire.Monad (Dev, iterRe)

start :: Dev Bool Bool
start = extrude (extrude (iterRe sig False) False) True

sig :: Bit -> ReacT Bit Bit (StateT Bit (StateT Bit Identity)) Bit
sig i = (if not i then incr else return ()) >>
        lift get >>= \ r0 ->
        signal r0

incr :: ReacT Bit Bit (StateT Bit (StateT Bit Identity)) ()
incr = lift get >>=~ \ r0 ->
       lift (lift get) >>= \ r1 ->
       lift (put r1) >>
       lift (lift (put (r0 `xor` r1)))

main = undefined