import ReWire ( signal, Identity, ReacT, StateT, lift, get, put)
import ReWire.Monad (Dev, iterRe, extrudeDev)
import ReWire.Bits (xor)

start :: Dev Bool Bool
start = extrudeDev (iterRe loop True) True

loop :: Bool -> ReacT Bool Bool (StateT Bool Identity) Bool
loop i = lift (stateAction i) >>= signal

stateAction :: Bool -> StateT Bool Identity Bool
stateAction i = get >>= \ s -> put (i `xor` s) >> return s

main = undefined