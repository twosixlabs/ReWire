{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^), (+), (==), (&&))
import ReWire.Bits
import ReWire

type W6    = W 6
type W8    = W 8
data Reg   = R0 | R1 | R2 | R3 
type Addr  = W6
data Instr = NOP | LD Addr | ST Addr | NAND Reg Reg Reg | BNZ Addr

data Ins     = Ins { instrIn :: Instr,
                     dataIn  :: W8 }
data Out     = Out { weOut   :: Bit,
                     addrOut :: Addr,
                     dataOut :: W8 }
data RegFile = RegFile { r0 :: W8, r1 :: W8, r2 :: W8, r3 :: W8,
                         pc :: Addr, inputs :: Ins, outputs :: Out }

type Dev = ReacT Ins Out (StateT RegFile Identity)
                      
getPC :: Dev Addr
getPC = do s <- lift get
           return (pc s)
           
putPC :: Addr -> Dev ()
putPC a = do s <- lift get
             lift (put (s { pc = a }))

incrPC :: Dev ()
incrPC = do pc <- getPC
            putPC (pc + lit 1)

getReg :: Reg -> Dev W8
getReg R0 = lift (get >>= return . r0) 
getReg R1 = lift (get >>= return . r1) 
getReg R2 = lift (get >>= return . r2) 
getReg R3 = lift (get >>= return . r3) 
            
putReg :: Reg -> W8 -> Dev ()
putReg R0 b = lift (get >>= \ s -> put (s { r0 = b }))
putReg R1 b = lift (get >>= \ s -> put (s { r1 = b }))
putReg R2 b = lift (get >>= \ s -> put (s { r2 = b }))
putReg R3 b = lift (get >>= \ s -> put (s { r3 = b }))

getOut :: Dev Out
getOut   = do
             s <- lift get
             return (outputs s) 

putOut :: Out -> Dev ()
putOut o = do
             s <- lift get
             lift (put (s { outputs = o }))

getIns :: Dev Ins
getIns   = do
             s <- lift get
             return (inputs s)

getDataIn :: Dev W8
getDataIn = do
              i <- getIns
              return (dataIn i) 

getInstr :: Dev Instr
getInstr = do
             i <- getIns
             return (instrIn i) 

putIns :: Ins -> Dev ()
putIns i = do
             s <- lift get
             lift (put (s { inputs = i })) 

tick :: Dev ()
tick     = do o <- getOut
              i <- signal o
              putIns i

putWeOut :: Bit -> Dev ()
putWeOut b = do o <- getOut
                putOut (o { weOut = b })

putAddrOut :: Addr -> Dev ()
putAddrOut a = do o <- getOut
                  putOut (o { addrOut = a })

putDataOut :: W8 -> Dev ()
putDataOut d = do o <- getOut
                  putOut (o { dataOut = d })

finishInstr :: Dev ()
finishInstr = do pc <- getPC
                 putAddrOut pc
                 putWeOut zero
                 tick

-- | Microcode

nop :: Dev ()
nop  = do
         incrPC
         finishInstr

ld :: Addr -> Dev ()
ld a = do
         putAddrOut a
         putWeOut zero
         tick
         incrPC
         finishInstr
         d <- getDataIn
         putReg R0 d

st :: Addr -> Dev ()
st a = do
         d <- getReg R0
         putAddrOut a
         putDataOut d
         putWeOut one
         tick
         incrPC
         finishInstr

nand :: Reg -> Reg -> Reg -> Dev ()
nand rd ra rb = do
                  a <- getReg ra
                  b <- getReg rb
                  putReg rd (bnot (a .&. b))
                  incrPC
                  finishInstr

bnz :: Addr -> Dev ()
bnz a = do
          v <- getReg R0
          if v == lit 0 then incrPC else putPC a
          finishInstr

loop :: Dev ()
loop = do instr <- getInstr
          s <- lift get
          case instr of
            NOP           -> nop
            LD a          -> ld a
            ST a          -> st a
            NAND rd r1 r2 -> nand rd r1 r2
            BNZ a         -> bnz a
          loop

reset :: Dev ()
reset = do putPC (lit 0)
           putDataOut (lit 0)
           tick
           finishInstr
           loop

start :: ReacT Ins Out Identity ()
start = extrude reset initState
   where
     initOut :: Out
     initOut = Out { weOut = zero,
                             addrOut = lit 0,
                             dataOut = lit 0 }
     initIns :: Ins
     initIns  = Ins  { instrIn = NOP,
                             dataIn = lit 0 }
     initState :: RegFile
     initState = RegFile { r0 = lit 0,
                            r1 = lit 0,
                            r2 = lit 0,
                            r3 = lit 0,
                            pc = lit 0,
                            outputs = initOut,
                            inputs  = initIns }
