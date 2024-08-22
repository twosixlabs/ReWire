import ReWire ( signal, Identity, ReacT )
import ReWire.Monad (Dev, iterRe)

start :: Dev Bool Bool
start = device True

device :: Bool -> Dev Bool Bool
device = iterRe loop

loop :: Bool -> ReacT Bool Bool Identity Bool
loop i = return (not i) >>= signal

main = undefined