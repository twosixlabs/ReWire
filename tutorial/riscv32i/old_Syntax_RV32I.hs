module Syntax_RV32I where

import Boilerplate -- not sure if I need this

-- Here, what I'm trying to do is square 
--  1. what we did before,
--  2. the assembly language output of riscv64-unknown-elf-gcc, and
--  3. the Patterson/Waterman RISC-V text.

-- type W20 = Int
-- type W12 = Int
-- type W5  = Int

-- See PW, Fig. 3.2., page 34.
data Register = Zero                        -- hardwired 0
              | RA                          -- return address
              | SP                          -- stack pointer
              | GP                          -- global pointer
              | TP                          -- thread pointer
              | T0                          -- temp., alt. link reg.
              | T1 | T2                     -- temporaries
              | S0                          -- aka, FP
              | S1                          -- saved register
              | A0 | A1                     -- fun. args/return vals
              | A2 | A3 | A4 | A5 | A6 | A7 -- fun args
              | S2 | S3 | S4 | S5 | S6 | S7 -- saved registers
              | S8 | S9 | S10 | S11 
              | T3 | T4 | T5 | T6           -- temporaries
            deriving Eq


instance Show Register where
   show Zero = "zero"
   show RA   = "ra"
   show SP   = "sp"
   show GP   = "gp"
   show TP   = "tp"
   show T0   = "t0"
   show T1   = "t1"
   show T2   = "t2"
   show S0   = "s0"
   show S1   = "s1"
   show A0   = "a0"
   show A1   = "a1"
   show A2   = "a2"
   show A3   = "a3"
   show A4   = "a4"
   show A5   = "a5"
   show A6   = "a6"
   show A7   = "a7"
   show S2   = "s2"
   show S3   = "s3"
   show S4   = "s4"
   show S5   = "s5"
   show S6   = "s6"
   show S7   = "s7"
   show S8   = "s8"
   show S9   = "s9"
   show S10  = "s10"
   show S11  = "s11"
   show T3   = "t3"
   show T4   = "t4"
   show T5   = "t5"
   show T6   = "t6"

------------------------
------------ Instructions
------------------------

data Instr = Addi  Register Register (LitArg W12)
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
           | Bne   Register Register W12
           | Blt   Register Register String
           | Bltu  Register Register W12
           | Bleu  Register Register W12
           | Bgt   Register Register W12
           | Bgtu  Register Register W12
           | Bge   Register Register W12
           | Bgeu  Register Register W12
           | Lui   Register (LitArg W20)
           | Auipc Register W20
           | Load  Register Register W12
           | Store Register Register W12
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
   -- below are new pseudo instructions. I need to keep the label around to calculate the CFG.
           | J String 
           | Call String 
           | JR Register
           | Bltz Register String 
           | Ble Register Register String -- W12
           | Li Register (LitArg W20) --- load immediate: addi rd x0 imm
                   deriving Eq

show3 oper arg1 arg2 arg3 = oper ++ " " ++ show arg1 ++ "," ++ show arg2 ++ "," ++ show arg3

instance Show Instr where
   show (Addi reg1 reg2 litarg) = show3 "addi" reg1 reg2 litarg
   show (Add reg1 reg2 reg3) = show3 "add" reg1 reg2 reg3
   show (Mul reg1 reg2 reg3) = show3 "mul" reg1 reg2 reg3
   show (Sub reg1 reg2 reg3) = show3 "sub" reg1 reg2 reg3
   show (And reg1 reg2 reg3) = show3 "and" reg1 reg2 reg3
   show (Or reg1 reg2 reg3) = show3 "or" reg1 reg2 reg3
   show (Xor reg1 reg2 reg3) = show3 "xor" reg1 reg2 reg3
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
   show (Bne reg1 reg2 w12) = show3 "bne" reg1 reg2 w12
   show (Ble reg1 reg2 w12) = show3 "ble" reg1 reg2 w12
   show (Blt reg1 reg2 str) = "blt " ++ show reg1 ++ "," ++ show reg2 ++ "," ++ str
   show (Bltu reg1 reg2 w12) = show3 "bltu" reg1 reg2 w12
   show (Bleu reg1 reg2 w12) = show3 "bleu" reg1 reg2 w12
   show (Bgt reg1 reg2 w12) = show3 "bgt" reg1 reg2 w12
   show (Bgtu reg1 reg2 w12) = show3 "bgtu" reg1 reg2 w12
   show (Bge reg1 reg2 w12) = show3 "bge" reg1 reg2 w12
   show (Bgeu reg1 reg2 w12) = show3 "bgeu" reg1 reg2 w12
   show (Lui reg litarg) = "lui " ++ show reg ++ "," ++ show litarg
   show (Auipc reg w20) = "auipc " ++ show reg ++ "," ++ show w20
   show (Load reg1 reg2 w12) = show3 "lw" reg1 reg2 w12
   show (Store reg1 reg2 w12) = show3 "sw" reg1 reg2 w12
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

           
data LitArg w = Hi String | Lo String | Word w deriving Eq

instance Show w => Show (LitArg w) where
  show (Hi s)   = "%hi(" ++ s ++ ")"
  show (Lo s)   = "%lo(" ++ s ++ ")"  
  show (Word w) = show w
