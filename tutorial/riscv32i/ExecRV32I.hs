{-# LANGUAGE DataKinds #-}
module ExecRV32I where

import Prelude hiding ((^), (+), (==), (&&), (<))
import ReWire
import ReWire.Bits 
import ReWire.Vectors

import BasicRV32I
import SyntaxRV32I

       ---------------------------------------------------------
       --            Asynchronous instruction fetch           --
       ---------------------------------------------------------

data Port a       = Val a | DC | Cmp
type InSig w i    = (Port w,Port i)
type OutSig a w e = (Port a,Port a,Port w,Port e)

fetch_ :: Port a -> OutSig a w e -> OutSig a w e
fetch_ oi (_,da,dw,e)  = (oi,da,dw,e)

_instr  :: InSig w i -> Port i
_instr (_,pi)  = pi

fetchinstr :: a -> OutSig a w e
fetchinstr a = fetch_ (Val a) blank

blank :: OutSig a w e
blank = (DC,DC,DC,DC)

async_fetch :: Monad m => a -> ReacT (InSig w i) (OutSig a w e) m i
async_fetch a = do
     i <- signal (fetchinstr a)
     async_wait_fetch a i

async_wait_fetch :: Monad m => a -> InSig w i -> ReacT (InSig w i) (OutSig a w e) m i
async_wait_fetch a i = case _instr i of
       Val w -> return w
       _     -> do
                  i <- signal (fetchinstr a)
                  async_wait_fetch a i

tick :: Monad m => ReacT i o (StateT registers (StateT (i,o) m)) ()
tick = do
  (_,o) <- lift (lift get)
  i <- signal o
  lift (lift (put (i,o)))

       ---------------------------------------------------------
       --                fetch-decode-execute                 --
       ---------------------------------------------------------

rv32i :: Monad m =>
         ReacT
            (InSig w (Instr))
            (OutSig W32 w e)
            (StateT RegFile (StateT (InSig w (Instr),OutSig W32 w e) m))
            ()
rv32i = do
  pc <- lift $ getReg PC
  iw <- async_fetch pc
  exec iw
  rv32i

exec :: Monad m => Instr -> ReacT i o (StateT RegFile (StateT (i, o) m)) ()
exec c = case c of

     Add rd rs1 rs2   -> do -- R-type
       lift $ do
                rs1  <- getReg rs1
                rs2  <- getReg rs2
                putReg rd (rs1 + rs2)
       tick

     Addi rd rs1 w12  -> do
       lift $ do
                rs   <- getReg rs1
                putReg rd (rs + resize w12)  -- does resize sign-extend? if not, this
       tick                                  -- needs work.

     And rd rs1 rs2   -> do
         lift $ do
                  v1     <- getReg rs1
                  v2     <- getReg rs2
                  putReg rd (v1 .&. v2)
         tick

     Or rd rs2 rs1   -> do
         lift $ do
                  v1     <- getReg rs1
                  v2     <- getReg rs2
                  putReg rd (v1 .|. v2)
         tick

     Xor rd rs2 rs1  -> do
         lift $ do
                   v1     <- getReg rs1
                   v2     <- getReg rs2
                   putReg rd (v1 ^ v2)
         tick

     -- Note: the microcode for Slti, Sltiu, Slt, and Sltu both use <. Two of
     -- these definitions are, therefore, incorrect. The spec distinguishes signed and
     -- unsigned less-than.
     -- \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

     Slti rd rs1 imm   -> do
         lift $ do let imm32 :: W 32 = sext imm
                   rs <- getReg rs1
                   putReg rd (resize (singleton (imm32 < rs))) 
         tick

     Sltiu rd rs1 w5   -> do
         lift $ do let imm32 :: W 32 = sext w5
                   rs <- getReg rs1
                   putReg rd (resize (singleton (imm32 < rs))) 
         tick

     Slt rd rs2 rs1    -> do
         lift $ do
                   v1 <- getReg rs1
                   v2 <- getReg rs2
                   putReg rd (resize (singleton (v2 < v1)))
         tick


     Sltu rd rs2 rs1  -> do
         lift $ do v1 <- getReg rs1
                   v2 <- getReg rs2
                   putReg rd (resize (singleton (v2 < v1)))
         tick

     -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     -- Note: the microcode for Slti, Sltiu, Slt, and Sltu both use <. Two of
     -- these definitions are, therefore, incorrect. The spec distinguishes signed and
     -- unsigned less-than.

     Andi rd rs1 w12   -> do
         lift $ do let imm :: W 32 = sext w12
                   srcVal    <- getReg rs1
                   putReg rd (imm .&. srcVal)
         tick

     Ori rd rs1 w12    -> do
         lift $ do let imm :: W 32 = sext w12
                   srcVal    <- getReg rs1
                   putReg rd (imm .|. srcVal)
         tick

     Xori rd rs1 w12 -> do
          lift $ do let imm :: W 32 = sext w12
                    srcVal    <- getReg rs1
                    putReg rd (imm ^ srcVal)
          tick

     Slli rd rs1 shamt  -> do
           lift $ do 
                     rs       <- getReg rs1
                     putReg rd (rs <<. resize shamt)
           tick

     Srli rd rs1 shamt  -> do
           lift $ do 
                     rs <- getReg rs1
                     putReg rd (rs >>. resize shamt)
           tick

     Srai rd rs1 shamt  -> do
           lift $ do 
                     rs <- getReg rs1
                     putReg rd (rs >>> resize shamt)
           tick

     Lui rd w20         -> do
           lift $ putReg rd (sext w20)
           tick

{- %%%%%%%%%%%%%%%%%%%%%%% Cordon Sanitaire

--     Auipc rd w20                   -> do
--       let imm = lui w20
--       pc      <- getReg PC
--       putReg rd (snd $ addW32 imm pc C)
--       tick




--     Sll rd rs2 rs1  -> do -- shifts use a "specialization" of I type.
--       rs    <- getReg rs1
--       shift <- getReg rs2
--       putReg rd (snd $ logShiftLW32 (C, rs) (w5ToInteger $ w32tow5 shift))
--       tick

--     Srl rd rs2 rs1  -> do -- shifts use a "specialization" of I type.
--       rs    <- getReg rs1
--       shift <- getReg rs2
--       putReg rd (snd $ logShiftRW32 (C, rs) (w5ToInteger $ w32tow5 shift))
--       tick

--     Sub rd rs2 rs1  -> do --    "
--       rs1  <- getReg rs1
--       rs2  <- getReg rs2
--       putReg rd (snd $ subW32 rs1 rs2 C)
--       tick

--     Sra rd rs2 rs1  -> do --    "
--       rs    <- getReg rs1
--       shift <- getReg rs2
--       putReg rd (snd $ arthShiftRW32 (C, rs) (w5ToInteger $ w32tow5 shift))
--       tick

--   --  Nop rd rs1 w12          -> tick

-- -- Section 2.5

--     Jal rd w20                    -> do
--       let imm = lui w20
--       pc            <- getReg PC
--       let pc'       = snd $ addW32 imm pc C
--           pcFINAL   = snd $ addW32 imm pc' C
--       putReg rd (snd $ addW32 pc (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S C C) C)
--       putReg PC pcFINAL
--       tick

--     Jalr rd rs1 w12         -> do
--       let imm = signextW12_to_W32 w12
--       rs1Val  <- getReg rs1
--       let res       = msbZero $ snd $ addW32 rs1Val imm C
--       pc            <- getReg PC
--       putReg rd (snd $ addW32 pc (W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S C C) C)
--       putReg PC res
--       tick

--     Beq src2 src1 w12  -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let pc'        = if (v1 == v2) then snd $ addW32 pc imm C else pc
--       putReg PC pc'
--       tick

--     Bne src2 src1 w12  -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let pc' = if v1==v2 then pc else snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Blt src2 src1 w12 -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result = sLessThan v1 v2
--       let pc'    = case isZero result of
--                     S -> pc
--                     C -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Bge src2 src1 w12 -> do                           --    ^^^ if you don't use the var, don't define it.
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result = sLessThan v1 v2
--       let pc'    = case isZero result of
--                     C -> pc
--                     S -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Bltu src2 src1 w12 -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result = uLessThan v1 v2
--       let pc'    = case isZero result of
--                     S -> pc
--                     C -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Bgeu src2 src1 w12 -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result      = uLessThan v1 v2
--       let pc'         = case isZero result of
--                          C -> pc
--                          S -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Ble src2 src1 w12  -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result = sLessThanEqual v1 v2
--       let pc'    = case isZero result of
--                     S -> pc
--                     C -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Bleu src2 src1 w12 -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result = uLessThanEqual v1 v2
--       let pc'    = case isZero result of
--                     S -> pc
--                     C -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Bgt src2 src1 w12 -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result      = sLessThanEqual v1 v2  -- check the logic here
--       let pc'         = case isZero result of
--                          C -> pc
--                          S -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

--     Bgtu src2 src1 w12 -> do
--       let imm  = signextW12_to_W32 w12
--       pc <- getReg PC
--       v1 <- getReg src1
--       v2 <- getReg src2
--       let result = uLessThanEqual v2 v1 -- check the logic
--       let pc'    = case isZero result of
--                     C -> pc
--                     S -> snd $ addW32 pc imm C
--       putReg PC pc'
--       tick

-- -- Section 2.6 Instruction
--     Load rd rs1 w12  -> do
--       let imm = signextW12_to_W32 w12
--       rs     <- getReg rs1
--       let off = snd $ addW32 imm rs C
--       result <- dataread_sync off
--       putReg rd result

--     Store src2 src1 w12 -> do
--       let imm  = signextW12_to_W32 w12
--       v1  <- getReg src1
--       v2  <- getReg src2
--       let effectAddr = snd $ addW32 imm v1 C
--       datawrite_sync effectAddr v2

-- -- Section 2.7 Instruction
--     Fence rd rs1 w12 -> undefined
--     Fencei rd rs1 w12    -> undefined

-- -- Section 2.8 Instruction
--     Csrrw w12 rd w5     -> undefined
--     Csrrs w12 rd w5     -> undefined
--     Csrrc w12 rd w5     -> undefined
--     Csrrwi w12 rd w5    -> undefined -- decode rs1 different?
--     Csrrsi w12 rd w5    -> undefined -- decode rs1 different?
--     Csrrci w12 rd w5    -> undefined -- decode rs1 different?
--     Rdcycle rd rs1 w12   -> undefined
--     Rdtime rd rs1 w12    -> undefined
--     Rdinstret rd rs1 w12 -> undefined

-- -- Section 2.9 Instruction
--     Ecall rd rs1 w12  -> undefined
--     Ebreak rd rs1 w12 -> undefined

%%%%%%%%%%%%%%%%%%%%%%%%%%% End of Cordon Sanitaire
-}

{-


type M m w e = StateT RegFile (StateT (InSig w (Instr),OutSig W32 w e) m)

evalLit12 :: Monad m => [(String,Int)] -> Lit12 -> M m w e W12
evalLit12 rho (LitCon12 w12)     = return $ evalCon12 rho w12
evalLit12 rho (RegOff12 w12 reg) = do
  v <- getReg reg
  let w12' = evalCon12 rho w12
  let w12'' = signextW12_to_W32 w12'
  return (getLo $ snd $ addW32 v w12'' C)

evalCon12 :: [(String, Int)] -> Con12 -> W12
evalCon12 rho (Simp12 i) = getLo $ convWordW32 i
evalCon12 rho (Lo12 l)   = case lookup l rho of
  Just a  -> getLo $ convWordW32 a
  Nothing -> error "WTF?"



-}
