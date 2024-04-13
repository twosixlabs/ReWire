{-# LANGUAGE DataKinds #-}
-- module Tutorial.RISCV32i.SyntaxRV32I where
module SyntaxRV32I where

import BasicRV32I

-- Here, what I'm trying to do is square
--  1. what we did before,
--  2. the assembly language output of riscv64-unknown-elf-gcc, and
--  3. the Patterson/Waterman RISC-V text.

{-
Here are some examples from RV assembly language. There is no published syntax for RV assembly, so
I'm doing what seems reasonable.

28(sp)        as in: sw ra,28(sp)
%lo(.LC0)     as in: addi a5,a5,%lo(.LC0)
%lo(.LC0)(a5) as in: ld	a4,%lo(.LC0)(a5)
32            as in: addi sp,sp,32
%hi(.LC0)     as in: lui a5,%hi(.LC0)
...and they can all be in different sizes; e.g., ADDI takes a W12, LUI takes a W20.
-}


{-
getHi :: W32 -> W20
getHi (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = (W20 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12)
getLo :: W32 -> W12
getLo (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = (W12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
-}

------------------------
------------ Instructions
------------------------

-- | Page 120, Fig. 2.19, Patterson/Hennessy Comp Org and Design: RISCV edition
-- | clarifies

data Instr = Addi Register Register W12
           | Add   Register Register Register
           | Mul   Register Register Register
           | Sub   Register Register Register
           | And   Register Register Register
           | Or    Register Register Register
           | Xor   Register Register Register
           | Andi  Register Register W12
           | Ori   Register Register W12
           | Xori  Register Register W12
           | Sll   Register Register Register
           | Srl   Register Register Register
           | Sra   Register Register Register
           | Slli  Register Register W5 -- not sure if this should
           | Srli  Register Register W5 -- actually be a W12
           | Srai  Register Register W5 -- as last arg ?
           | Slt   Register Register Register
           | Sltu  Register Register Register
           | Slti  Register Register W5
           | Sltiu Register Register W5
           | Jal   Register W20
           | Jalr  Register Register W12
           | Beq   Register Register W12
           | Bne   Register Register String
           | Bnez  Register String
           | Blt   Register Register String
           | Bltu  Register Register W12
           | Bleu  Register Register W12
           | Bgt   Register Register W12
           | Bgtu  Register Register W12
           | Bge   Register Register W12
           | Bgeu  Register Register W12
           | Lui   Register W20
           | Auipc Register W20
           | Load  Register W12
           | Store Register W12
           | Fence Register  Register W12
           | Fencei Register Register W12
           | Csrrw W12 Register W5
           | Csrrs W12 Register W5
           | Csrrc W12 Register W5
           | Csrrwi W12 Register W5
           | Csrrsi W12 Register W5
           | Csrrci W12 Register W5
           | Rdcycle Register Register W12
           | Rdtime Register Register W12
           | Rdinstret Register Register W12
           | Ecall W12 Register W5
           | Ebreak W12 Register W5
           | Rem Register Register Register
  -- below are new pseudo instructions. I need to keep the label around to calculate the CFG.
           | J String
           | Call String
           | JR Register
           | Bltz Register String
           | Ble Register Register String -- W12
           | Li Register W20 --- load immediate: addi rd x0 imm
           | Not Register Register      --- implemented with xori.
           | Mv Register Register      -- mv is a pseudo instruction for "addi rd r1 0".
           
{-
show3 oper arg1 arg2 arg3 = oper ++ " " ++ show arg1 ++ "," ++ show arg2 ++ "," ++ show arg3
show3' oper arg1 arg2 arg3 = oper ++ " " ++ show arg1 ++ "," ++ show arg2 ++ "," ++ arg3

instance Show Instr where
   show (Addi reg1 reg2 litarg) = show3 "addi" reg1 reg2 litarg
   show (Add reg1 reg2 reg3) = show3 "add" reg1 reg2 reg3
   show (Mul reg1 reg2 reg3) = show3 "mul" reg1 reg2 reg3
   show (Sub reg1 reg2 reg3) = show3 "sub" reg1 reg2 reg3
   show (And reg1 reg2 reg3) = show3 "and" reg1 reg2 reg3
   show (Or reg1 reg2 reg3) = show3 "or" reg1 reg2 reg3
   show (Xor reg1 reg2 reg3) = show3 "xor" reg1 reg2 reg3
   show (Rem reg1 reg2 reg3) = show3 "rem" reg1 reg2 reg3
   show (Andi reg1 reg2 w12) = show3 "andi" reg1 reg2 w12
   show (Ori reg1 reg2 w12) = show3 "ori" reg1 reg2 w12
   show (Xori reg1 reg2 w12) = show3 "xori" reg1 reg2 w12
   show (Sll reg1 reg2 reg3) = show3 "sll" reg1 reg2 reg3
   show (Srl reg1 reg2 reg3) = show3 "srl" reg1 reg2 reg3
   show (Sra reg1 reg2 reg3) = show3 "sra" reg1 reg2 reg3
   show (Slli reg1 reg2 w5) = show3 "slli" reg1 reg2 w5
   show (Srli reg1 reg2 w5) = show3 "srli" reg1 reg2 w5
   show (Srai reg1 reg2 w5) = show3 "srai" reg1 reg2 w5
   show (Slt reg1 reg2 reg3) = show3 "slt" reg1 reg2 reg3
   show (Sltu reg1 reg2 reg3) = show3 "sltu" reg1 reg2 reg3
   show (Slti reg1 reg2 w5) = show3 "slti" reg1 reg2 w5
   show (Sltiu reg1 reg2 w5) = show3 "sltiu" reg1 reg2 w5
   show (Jal reg w20)        = "jal " ++ show reg ++ "," ++ show w20
   show (Jalr reg1 reg2 w12) = show3 "jalr" reg1 reg2 w12
   show (Beq reg1 reg2 w12) = show3 "beq" reg1 reg2 w12
   show (Bne reg1 reg2 w12) = show3' "bne" reg1 reg2 w12
   show (Bnez reg w12)      = "bnez " ++ show reg ++ "," ++ w12
   show (Ble reg1 reg2 w12) = show3' "ble" reg1 reg2 w12
   show (Blt reg1 reg2 str) = "blt " ++ show reg1 ++ "," ++ show reg2 ++ "," ++ str
   show (Bltu reg1 reg2 w12) = show3 "bltu" reg1 reg2 w12
   show (Bleu reg1 reg2 w12) = show3 "bleu" reg1 reg2 w12
   show (Bgt reg1 reg2 w12) = show3 "bgt" reg1 reg2 w12
   show (Bgtu reg1 reg2 w12) = show3 "bgtu" reg1 reg2 w12
   show (Bge reg1 reg2 w12) = show3 "bge" reg1 reg2 w12
   show (Bgeu reg1 reg2 w12) = show3 "bgeu" reg1 reg2 w12
   show (Lui reg litarg) = "lui " ++ show reg ++ "," ++ show litarg
   show (Auipc reg w20) = "auipc " ++ show reg ++ "," ++ show w20
   show (Load reg w12) = "lw " ++ show reg ++ "," ++ show w12
   show (Store reg w12)      = "sw " ++ show reg ++ "," ++ show w12
   show (Fence reg1 reg2 w12) = show3 "fence" reg1 reg2 w12
   show (Fencei reg1 reg2 w12) = show3 "fencei" reg1 reg2 w12
   show (Csrrw w12 reg w5) = show3 "csrrw" w12 reg w5
   show (Csrrs w12 reg w5) = show3 "csrrs" w12 reg w5
   show (Csrrc w12 reg w5) = show3 "csrrc" w12 reg w5
   show (Csrrwi w12 reg w5) = show3 "csrrwi" w12 reg w5
   show (Csrrsi w12 reg w5) = show3 "csrrsi" w12 reg w5
   show (Csrrci w12 reg w5) = show3 "csrrci" w12 reg w5
   show (Rdcycle reg1 reg2 w12) = show3 "rdcycle" reg1 reg2 w12
   show (Rdtime reg1 reg2 w12) = show3 "rdtime" reg1 reg2 w12
   show (Rdinstret reg1 reg2 w12) = show3 "rdinstret" reg1 reg2 w12
   show (Ecall w12 reg w5) = show3 "ecall" w12 reg w5
   show (Ebreak w12 reg w5) = show3 "ebreak" w12 reg w5
   -- pseudo instructions.
   show (J str) = "j " ++ str
   show (JR reg) = "jr " ++ show reg
   show (Call str) = "call " ++ str
   show (Bltz reg str) = "bltz " ++ show reg ++ "," ++ str
   show (Li reg litarg) = "li " ++ show reg ++ "," ++ show litarg
   show (Mv r1 r2)      = "mv " ++ show r1 ++ "," ++ show r2
-}

--------------
-- Junkyard below
--------------

-- data Con12 = Simp12 Int | Lo12 String deriving Eq
-- data Con20 = Simp20 Int | Hi20 String deriving Eq
-- data Lit12 = LitCon12 Con12 | RegOff12 Con12 Register deriving Eq
-- data Lit20 = LitCon20 Con20 | RegOff20 Con20 Register deriving Eq

-- instance Show Con12 where
--   show (Simp12 i) = show i
--   show (Lo12 s)   = "%lo(" ++ s ++ ")"

-- instance Show Con20 where
--   show (Simp20 i) = show i
--   show (Hi20 s)   = "%hi(" ++ s ++ ")"

-- instance Show Lit12 where
--   show (RegOff12 lit reg) = show lit ++ "(" ++ show reg ++ ")"
--   show (LitCon12 lit)        = show lit

-- instance Show Lit20 where
--   show (RegOff20 lit reg) = show lit ++ "(" ++ show reg ++ ")"
--   show (LitCon20 lit)         = show lit

-- convWordW20 l = do
--   let bits = intConv l
--   let wl   = intsToBits bits
--   let w    = listToW20 wl
--   w

-- convWordW5 l = do
--   let bits = intConv l
--   let wl   = intsToBits bits
--   let w    = listToW5 wl
--   w

-- convWordW12 l = do
--   let bits = intConv l
--   let wl   = intsToBits bits
--   let w    = listToW12 wl
--   w

-- convWordW32 l = do
--   let bits = intConv l
--   let wl   = intsToBits bits
--   let w    = listToW32 wl
--   w
