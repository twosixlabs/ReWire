module Parser_RV32I where

import Parsing
import Boilerplate as BPlate
import Syntax_RV32I
import Foreign.Storable
import Data.Bits
--import AST_RISCV

-- this is (the start of) a slap-dash, slapped together parser for RV32I assembly.

--data Bit = S | C deriving Show

parseRV32I p_ = do s <- readFile p_
                   let ls   = filter ignore $ map words $ lines s
                   let targ = map mkinstr ls
                   putStrLn $ pretty targ
                   
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

parseCommand :: Parser Instr
parseCommand =      parseAddi +++
                    parseMv   +++
                    parseSW   +++
                    parseLW   +++
                    pseudoLi   +++ 
                    parseLui  +++
                    pseudoJ   +++
                    pseudoJR  +++
                    parseSlli +++
                    parseAdd  +++
                    parseMul  +++
                    parseBlt  +++
                    pseudoBle  +++
                    pseudoBltz +++
                    pseudoCall
                    -- parseSub    +++
                    -- parseAnd    +++
                    -- parseOr     +++
                    -- parseXor    +++
                    -- parseXori   +++
                    -- parseOri    +++
                    -- parseAndi   +++
                    -- parseLui    +++
                    -- parseAuipc  +++
                    -- parseNot    -- +++   undefined

parseAddi :: Parser Instr
parseAddi = do
   symbol "addi"
   rd <- register
   symbol ","
   r1 <- register
   symbol ","
--   imm <- parseImm12
   litarg <- parseLitArg parseImm12
   return (Addi rd r1 litarg)

parseHi = do
  symbol "%hi("
  l <- label
  char ')'
  return (Hi l)

parseLo = do
  symbol "%lo("
  l <- label
  char ')'
  return (Lo l)

parseLitArg pw = parseHi +++ parseLo +++ (pw >>= return . Word)
  
-- mv is a pseudo instruction for "addi rd r1 0".
parseMv :: Parser Instr
parseMv = do
  symbol "mv"
  rd <- register
  symbol ","
  r1 <- register
  return (Addi rd r1 (Word (W12 C C C C C C C C C C C C)))

-- ex.,	sw a5,-28(s0)
parseSW = do
  symbol "sw"
  rd <- register
  symbol ","
  offset <- parseImm12
  symbol "("
  rs <- register
  symbol ")"
  return (Store rd rs offset)

-- ex.,	lw a5,-28(s0)
parseLW = do
  symbol "lw"
  rd <- register
  symbol ","
  offset <- parseImm12
  symbol "("
  rs <- register
  symbol ")"
  return (Load rd rs offset)

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


label :: Parser String
label = token lab +++ (many alphanum)
   where lab = do symbol "."
                  xs <- many alphanum
                  return ('.':xs)

vomit :: Parser String
vomit = token vom

vom = do xs <- many (char '.' +++ char '%' +++ char '(' +++ char ')'  +++ alphanum)
         return xs

sym i = do
  c <- symbol i
  return [c]
  
-- pseudo instruction for LI.
pseudoLi :: Parser Instr
pseudoLi = do
  symbol "li"
  rd <- register
  symbol ","
  imm <- parseImm20
  return (Li rd (Word imm))

parseLui :: Parser Instr
parseLui = do
  symbol "lui"
  rd <- register
  symbol ","
--  imm <- parseImm20
  litarg <- parseLitArg parseImm20
  return (Lui rd litarg) -- (Word imm))

parseSlli :: Parser Instr
parseSlli = do
  symbol "slli"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm5
  return (Slli rd r1 imm)

parseAdd :: Parser Instr
parseAdd = do
  symbol "add"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Add rd r1 r2)

parseMul :: Parser Instr
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

pseudoBltz = do
  symbol "bltz"
  rs1 <- register
  symbol ","
  l <- label
  return (Bltz rs1 l)

pseudoCall = do
  symbol "call"
  l <- label
  return (Call l)
  
--
--
--

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


---------------
-- For the code below, I make no claims that it is needed.
---------------

parseRV :: FilePath -> IO [Instr]
parseRV p_ = do s      <- readFile p_
                let pr = parse (many1 parseCommand) s
                case pr of
                   [] -> fail "Argghhh!"
                   [(ss,_)] -> return ss
                   _         -> fail "Too many parses"


parseImm12 :: Parser W12
parseImm12 = do
   n <- integer
   if n > 2048 || n < -1024
     then return $ error "immediate too large"
     else return $ bits_to_w12 $ int_to_12bits n


-- newparseImm12 :: Parser Int
-- newparseImm12 = do
--    n <- integer
--    if n > 2048 || n < -1024
--      then return $ error "immediate too large"
--      else return $ n

parseImm20 :: Parser W20
parseImm20 = do
     n <- integer
     if n > 2048 || n < -1024
       then return $ error "immediate too large"
       else return $ bits_to_w20 $ int_to_20bits n

parseImm5 :: Parser W5
parseImm5 = do
   n <- integer
   if n > 2048 || n < -1024
     then return $ error "immediate too large"
     else return $ bits_to_w5 $ int_to_5bits n

int_to_5bits :: Int -> [BPlate.Bit]
int_to_5bits i = reverse $ take 5 $ map bool2bit (map (testBit i) [0..8*(sizeOf i)-1])

bits_to_w5 :: [BPlate.Bit] -> W5
bits_to_w5 (b4:b3:b2:b1:b0:_)
  = (W5 b4 b3 b2 b1 b0)
bits_to_w5 _ = error "ITS OUT OF BOUNDS?"

int_to_12bits :: Int -> [BPlate.Bit]
int_to_12bits i = reverse $ take 12 $ map bool2bit (map (testBit i) [0..8*(sizeOf i)-1])

bits_to_w12 :: [BPlate.Bit] -> W12
bits_to_w12 (b11:b10:b9:b8:b7:b6:b5:b4:b3:b2:b1:b0:_)
   = (W12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
bits_to_w12 _ = error "ITS OUT OF BOUNDS?"

int_to_20bits :: Int -> [BPlate.Bit]
int_to_20bits i = reverse $ take 20 $ map bool2bit (map (testBit i) [0..8*(sizeOf i)-1])

bits_to_w20 :: [BPlate.Bit] -> W20
bits_to_w20 (b19:b18:b17:b16:b15:b14:b13:b12:b11:b10:b9:b8:b7:b6:b5:b4:b3:b2:b1:b0:_)
   = (W20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
bits_to_w20 _ = error "ITS OUT OF BOUNDS?"
-- source for instruction help
-- https://www.imperialviolet.org/2016/12/31/riscv.html

parseSub :: Parser Instr
parseSub = do
  symbol "sub"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sub rd r1 r2)

parseAnd :: Parser Instr
parseAnd = do
  symbol "and"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (And rd r1 r2)

parseOr :: Parser Instr
parseOr = do
  symbol "or"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Or rd r1 r2)

parseXor :: Parser Instr
parseXor = do
  symbol "xor"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Xor rd r1 r2)

parseAndi :: Parser Instr
parseAndi = do
  symbol "andi"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm12
  return (Andi rd r1 imm)

parseOri :: Parser Instr
parseOri = do
  symbol "ori"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm12
  return (Ori rd r1 imm)

parseXori :: Parser Instr
parseXori = do
  symbol "xori"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm12
  return (Xori rd r1 imm)

parseNop = error "Nop huh?"

-- parseNop :: Parser Instr
-- parseNop = do
--   symbol "nop"
--   return (Addi X0 X0 (W12 C C C C C C C C C C C C))

parseNot  :: Parser Instr
parseNot = do
  symbol "not"
  rd <- register
  symbol ","
  r1 <- register
  return (Xori rd r1 (W12 S S S S S S S S S S S S))
      
parseAuipc :: Parser Instr
parseAuipc = do
  symbol "lui"
  rd <- register
  symbol ","
  imm <- parseImm20
  return (Auipc rd imm)

parseSll :: Parser Instr
parseSll = do
  symbol "sll"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sll rd r1 r2)

parseSrl :: Parser Instr
parseSrl = do
  symbol "sll"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Srl rd r1 r2)

parseSra :: Parser Instr
parseSra = do
  symbol "sra"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sra rd r1 r2)

parseSrli :: Parser Instr
parseSrli = do
  symbol "srli"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm5
  return (Srli rd r1 imm)

parseSrai :: Parser Instr
parseSrai = do
  symbol "srai"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm5
  return (Srai rd r1 imm)

parseSlt :: Parser Instr
parseSlt = do
  symbol "slt"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Slt rd r1 r2)

parseSltu :: Parser Instr
parseSltu = do
  symbol "sltu"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  r2 <- register
  return (Sltu rd r1 r2)

parseSlti :: Parser Instr
parseSlti = do
  symbol "slti"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm5
  return (Slti rd r1 imm)

parseSltiu :: Parser Instr
parseSltiu = do
  symbol "sltiu"
  rd <- register
  symbol ","
  r1 <- register
  symbol ","
  imm <- parseImm5
  return (Sltiu rd r1 imm)




--                    parseNop    +++


-- =======
-- import System.Environment
-- import System.IO
-- import Boilerplate
-- import AST_RISCV

-- main :: IO ()
-- main = do
--   [f]   <- getArgs
--   h     <- readFile f
--   let wds   = parseWords (reverse $ lines h) []
--   executeProgram wds

-- parseWords :: [String] -> [W32] -> [W32]
-- parseWords [] acc           = acc
-- parseWords (x:xs) acc       = parseWords xs $ [w32fromString x] ++ acc

-- executeProgram :: [W32] -> IO ()
-- executeProgram []     = putStrLn "Program Finito"
-- executeProgram (x:xs) = do
--   let instr       = decode x
--   print instr
--   let whateverV2  = step instr
--   executeProgram xs
