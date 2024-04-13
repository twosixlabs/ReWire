module ParserRV32I where

import BasicRV32I
import Parsing
import SyntaxRV32I

-- run = do
--   s <- parseRV32I "tests/euclidsBasic32.s"
--   let a = load s
--   return a

load instrs = calc 0 instrs
  where
    calc _ [] = []
    calc acc (Left x:xs) = [(x,acc)] ++ calc acc xs
    calc acc (Right x:xs) = calc (acc+1) xs

-- parseRV32I
--   :: FilePath
--      -> IO
--           [Either
--              [Char]
--              (Instr)]
parseRV32I p_ = do s <- readFile p_
                   let ls   = filter ignore $ map words $ lines s
                   let targ = map mkinstr ls
--                   return targ
                   putStrLn $ pretty targ

pretty :: Show a => [Either [Char] a] -> [Char]
pretty []           = ""
pretty (Left l : k) = l ++ ":" ++ "\n" ++ pretty k
pretty (Right i : k) = "\t" ++ show i ++ "\n" ++ pretty k

mkinstr [lbl] = Left (reverse $ tail $ reverse lbl)
mkinstr [op,args] = Right $ instr
  where line  = op ++ " " ++ args
        instr = case parse parseCommand line of
                     [(i,[])] -> i
                     []       -> error $ "no parse: " ++ line
                     [(i,k)]  -> i -- error $ "incomplete parse: " ++ k
                     _        -> error "multiple parses"
mkinstr what  = error $ "Huh?: " ++ show what
--
-- Ignoring assembler directives.
--
ignore (h:_) = case h of
  ".align"   -> False
  ".file"    -> False
  ".option"  -> False
  ".text"    -> False
  ".size"    -> False
  ".globl"   -> False
  ".type"    -> False
  ".ident"   -> False
  ".section" -> False
  ".string"  -> False
  ".word"    -> False
  _          -> True

--
--
--

parseCommand =      parseAddi +++
                    parseMv   +++
                    parseSW   +++
                    parseLW   +++
                    pseudoLi   +++
                    parseLui  +++
                    pseudoJR  +++
                    pseudoJ   +++
                    parseSlli +++
                    parseAdd  +++
                    parseMul    +++
                    parseBne    +++
                    parseBnez   +++
                    parseBlt    +++
                    pseudoBle   +++
                    pseudoBltz  +++
                    pseudoCall  +++
                    parseSub    +++
                    parseAnd    +++
                    parseOr     +++
                    parseXor    +++
                    parseXori   +++
                    parseOri    +++
                    parseAndi   +++
                    parseLui    +++
                    parseAuipc   +++
                    parseRem    +++
                    pseudoNot


parseRV p_ = do s      <- readFile p_
                let pr = parse (many1 parseCommand) s
                case pr of
                   [] -> fail "Argghhh!"
                   [(ss,_)] -> return ss
                   _         -> fail "Too many parses"

iparse str = parse parseCommand str

-- things like ".L5"
label :: Parser String
label = token lab +++ (many alphanum)
   where lab = do symbol "."
                  xs <- many alphanum
                  return ('.':xs)


-- Here are some examples from RV assembly language. There is no published syntax for RV assembly, so
-- I'm doing what seems reasonable.

-- 28(sp)        as in: sw ra,28(sp)
-- %lo(.LC0)     as in: addi a5,a5,%lo(.LC0)
-- %lo(.LC0)(a5) as in: ld a4,%lo(.LC0)(a5)
-- 32            as in: addi sp,sp,32
-- %hi(.LC0)     as in: lui a5,%hi(.LC0)
-- ...and they can all be in different sizes; e.g., ADDI takes a W12, LUI takes a W20.

w5 :: Parser W5
w5 = integer >>= return . convWordW5

parseLit12 :: Parser Lit12
parseLit12 = (parseCon12 >>= return . LitCon12) +++ regoffset12
    where
      regoffset12 :: Parser Lit12
      regoffset12 = do
        l <- parseCon12
        char '('
        r <- register
        char ')'
        return $ RegOff12 l r
      parseCon12 = (integer >>= return . Simp12) +++ parseLo12
      parseLo12 = do
         symbol "%lo("
         l <- label
         char ')'
         return (Lo12 l)

parseLit20 :: Parser Lit20       
parseLit20 = (parseCon20 >>= return . LitCon20) +++ regoffset20
    where
      regoffset20 :: Parser Lit20
      regoffset20 = do
        l <- parseCon20
        char '('
        r <- register
        char ')'
        return $ RegOff20 l r
      parseCon20 = (integer >>= return . Simp20) +++ parseHi20
      parseHi20 = do
         symbol "%hi("
         l <- label
         char ')'
         return (Hi20 l)

register :: Parser Register
register = parseZero +++
           parseSP   +++
           parseS0   +++
           parseRA   +++
           parseGP   +++
           parseTP   +++
           parseAReg +++
           parseTReg +++
           parseSReg

parseZero = symbol "zero" >> return Zero
parseSP   = symbol "sp" >> return SP
parseS0   = symbol "s0" >> return S0
parseRA   = symbol "ra" >> return RA
parseGP   = symbol "gp" >> return GP
parseTP   = symbol "tp" >> return TP

parseAReg :: Parser Register
parseAReg = do
   char 'a'
   n <- natural
   mkAreg n
     where
       mkAreg 0 = return A0
       mkAreg 1 = return A1
       mkAreg 2 = return A2
       mkAreg 3 = return A3
       mkAreg 4 = return A4
       mkAreg 5 = return A5
       mkAreg 6 = return A6
       mkAreg 7 = return A7
       mkAreg n = error $ "No A register for " ++ show n

parseTReg :: Parser Register
parseTReg = do
   char 't'
   n <- natural
   mkTreg n
     where
       mkTreg 0 = return T0
       mkTreg 1 = return T1
       mkTreg 2 = return T2
       mkTreg 3 = return T3
       mkTreg 4 = return T4
       mkTreg 5 = return T5
       mkTreg 6 = return T6
       mkTreg n = error $ "No T register for " ++ show n

parseSReg :: Parser Register
parseSReg = do
   char 's'
   n <- natural
   mkSreg n
     where
       mkSreg 0  = return S0
       mkSreg 1  = return S1
       mkSreg 2  = return S2
       mkSreg 3  = return S3
       mkSreg 4  = return S4
       mkSreg 5  = return S5
       mkSreg 6  = return S6
       mkSreg 7  = return S7
       mkSreg 8  = return S8
       mkSreg 9  = return S9
       mkSreg 10 = return S10
       mkSreg 11 = return S11
       mkSreg n  = error $ "No S register for " ++ show n

---
---
---

parseSub :: Parser (Instr)
parseSub = do
  symbol "sub"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sub rd r1 r2)

parseAnd :: Parser (Instr)
parseAnd = do
  symbol "and"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (And rd r1 r2)

parseOr :: Parser (Instr)
parseOr = do
  symbol "or"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Or rd r1 r2)

parseXor :: Parser (Instr)
parseXor = do
  symbol "xor"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Xor rd r1 r2)

--parseAndi :: Parser (Instr String w5 Int w20)
parseAndi = do
  symbol "andi"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseLit12
  return (Andi rd r1 imm)

--parseOri :: Parser (Instr l w5 (LitArg Int) w20)
--parseOri :: Parser (Instr String w5 Int w20)
parseOri = do
  symbol "ori"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseLit12
  return (Ori rd r1 imm)

--parseXori :: Parser (Instr l w5 (Literal Int) w20)
parseXori = do
  symbol "xori"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseLit12
  return (Xori rd r1 imm)

parseNop = error "Nop huh?"

-- parseNop :: Parser Instr
-- parseNop = do
--   symbol "nop"
--   return (Addi X0 X0 (W12 C C C C C C C C C C C C))

pseudoNot = do
  symbol "not"
  rd <- register
  symbol ","
  r1 <- register
  return (Not rd r1)

parseAuipc = do
  symbol "lui"
  rd <- register
  symbol ","
  imm <- parseLit20
  return (Auipc rd imm)

parseSll = do
  symbol "sll"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sll rd r1 r2)


parseSrl = do
  symbol "sll"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Srl rd r1 r2)


parseSra = do
  symbol "sra"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sra rd r1 r2)


parseSrli = do
  symbol "srli"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- w5
  return (Srli rd r1 imm)


parseSrai = do
  symbol "srai"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- w5
  return (Srai rd r1 imm)


parseSlt = do
  symbol "slt"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Slt rd r1 r2)


parseSltu = do
  symbol "sltu"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sltu rd r1 r2)


parseSlti = do
  symbol "slti"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- w5
  return (Slti rd r1 imm)


parseSltiu = do
  symbol "sltiu"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- w5
  return (Sltiu rd r1 imm)

-- pseudo instruction for LI.
pseudoLi = do
  symbol "li"
  rd <- register
  symbol ","
  imm <- parseLit20
  return (Li rd imm)

-- ex.,	sw a5,-28(s0)
pseudoJ = do
  symbol "j"
  l <- label
  return (J l)

-- ex.,	jr ra
pseudoJR = do
  symbol "jr"
  ra <- register
  return (JR ra)

parseAddi = do
   symbol "addi"
   rd <- register
   symbol ","
   r1 <- register
   symbol ","
   la <- parseLit12
   return (Addi rd r1 la)

-- mv is a pseudo instruction for "addi rd r1 0".
parseMv = do
  symbol "mv"
  rd <- register
  symbol ","
  r1 <- register
  return (Mv rd r1)
--  return (Addi rd r1 (Word (W12 C C C C C C C C C C C C)))

-- ex.,	sw a5,-28(s0)
parseSW = do
  symbol "sw"
  rd <- register
  symbol ","
  la <- parseLit12
  return (Store rd la)


-- ex.,	lw a5,-28(s0)
parseLW = do
  symbol "lw"
  rd <- register
  symbol ","
  la <- parseLit12
  return (Load rd la)


parseLui = do
  symbol "lui"
  rd <- register
  symbol ","
--  imm <- parseImm20
  la <- parseLit20
  return (Lui rd la) -- (Word imm))


parseSlli = do
  symbol "slli"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- w5
  return (Slli rd r1 imm)

parseAdd = do
  symbol "add"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Add rd r1 r2)

parseRem = do
  symbol "rem"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Rem rd r1 r2)


parseMul = do
  symbol "mul"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Mul rd r1 r2)


parseBlt = do
  symbol "blt"
  rs1 <- register
  symbol ","
  rs2 <- register
  symbol ","
  l <- label
  return (Blt rs1 rs2 l)

--	ble	a4,a5,.L3
pseudoBle = do
  symbol "ble"
  rs1 <- register
  symbol ","
  rs2 <- register
  symbol ","
  l <- label
  return (Ble rs1 rs2 l)

pseudoCall :: Parser (Instr)
pseudoCall = do
  symbol "call"
  l <- label
  return (Call l)

parseBne = do
  symbol "bne"
  rs <- register
  symbol ","
  rd <- register
  symbol ","
  l <- label
  return (Bne rs rd l)

parseBnez = do
  symbol "bnez"
  rs <- register
  symbol ","
  l <- label
  return (Bnez rs l)

pseudoBltz = do
  symbol "bltz"
  rs1 <- register
  symbol ","
  l <- label
  return (Bltz rs1 l)
