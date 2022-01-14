{-# LANGUAGE Trustworthy, OverloadedStrings #-}
module ReWire.Verilog.Syntax where

import Prettyprinter (Pretty (..), parens, (<+>), vsep, hsep, semi, colon, punctuate, comma, nest, Doc, braces, brackets)
import ReWire.Pretty (empty, text)
import Data.Text (Text, pack)
import Data.List (intersperse)
import Data.BitVector (BV (..), showHex, width, ones, zeros)

newtype Program = Program { pgmModules :: [Module] }
      deriving (Eq, Show)

instance Pretty Program where
      pretty (Program mods) = vsep (intersperse empty $ map pretty mods)

type Name  = Text
type Size  = Word
type Index = Int
type Value = Integer

data Module = Module
      { modName    :: Name
      , modPorts   :: [Port]
      , modSignals :: [Signal]
      , modStmt    :: [Stmt]
      }
      deriving (Eq, Show)

instance Pretty Module where
      pretty (Module n ps sigs stmt) = text "module" <+> text n
            <+> nest 2 (parens $ vsep $ punctuate comma $ map pretty ps)
             <> vsep [nest 2 (vsep ([semi] <> map ((<> semi) . pretty) sigs <> map pretty stmt)), text "endmodule"]

data Port = Input  Signal -- can't be reg
          | InOut  Signal -- can't be reg
          | Output Signal
      deriving (Eq, Show)

instance Pretty Port where
      pretty = \ case
            Input s  -> text "input" <+> pretty s
            InOut s  -> text "inout" <+> pretty s
            Output s -> text "output" <+> pretty s

ppBVName :: Size -> Name -> Doc an
ppBVName sz n = brackets (pretty (toInteger sz - 1) <> colon <> text "0") <+> text n

data Signal = Wire Size Name
            | Logic Size Name
            | Reg  Size Name
      deriving (Eq, Show)

instance Pretty Signal where
      pretty = \ case
            -- Wire 1 n   -> text "wire" <+> text n
            Wire sz n  -> text "wire" <+> ppBVName sz n
            -- Logic 1 n  -> text "logic" <+> text n
            Logic sz n -> text "logic" <+> ppBVName sz n
            -- Reg 1 n    -> text "reg"  <+> text n
            Reg sz n   -> text "reg"  <+> ppBVName sz n

data Stmt = Always [Sensitivity] Stmt
          | Initial Stmt
          | IfElse Exp Stmt Stmt
          | If Exp Stmt
          | Assign LVal Exp
          | SeqAssign LVal Exp
          | ParAssign LVal Exp
          | Block [Stmt]
          | Instantiate Name Name [Exp] -- [Exp] for convenience, should be [LVal]?
      deriving (Eq, Show)

instance Pretty Stmt where
      pretty = \ case
            Always sens stmt      -> text "always" <+> text "@" <+> parens (hsep $ punctuate (text " or") $ map pretty sens) <+> pretty stmt
            Initial stmt          -> text "initial" <+> pretty stmt
            IfElse c thn els      -> text "if" <+> parens (pretty c) <+> pretty thn <+> text "else" <+> pretty els
            If c thn              -> text "if" <+> parens (pretty c) <+> pretty thn
            Assign lv v           -> text "assign" <+> pretty lv <+> text "="  <+> pretty v <> semi
            SeqAssign lv v        ->                   pretty lv <+> text "="  <+> pretty v <> semi
            ParAssign lv v        ->                   pretty lv <+> text "<=" <+> pretty v <> semi
            Block stmts           -> vsep [nest 2 $ vsep (text "begin" : map pretty stmts), text "end"]
            Instantiate m inst ss -> text m <+> text inst <+> parens (hsep $ punctuate comma $ map pretty ss) <> semi

data Sensitivity = Pos Name | Neg Name
      deriving (Eq, Show)

instance Pretty Sensitivity where
      pretty = \ case
            Pos n -> text "posedge" <+> text n
            Neg n -> text "negedge" <+> text n

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Pow Exp Exp
         | LAnd Exp Exp
         | LOr Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | XOr Exp Exp
         | XNor Exp Exp
         | LShift Exp Exp
         | RShift Exp Exp
         | LShiftArith Exp Exp
         | RShiftArith Exp Exp
         | Not Exp
         | LNot Exp
         | RAnd Exp
         | RNAnd Exp
         | ROr Exp
         | RNor Exp
         | RXor Exp
         | RXNor Exp
         | Eq  Exp Exp
         | NEq Exp Exp
         | CEq  Exp Exp
         | CNEq Exp Exp
         | Lt  Exp Exp
         | Gt  Exp Exp
         | LtEq  Exp Exp
         | GtEq  Exp Exp
         | Cond Exp Exp Exp
         | Concat [Exp]
         | Repl Size Exp
         | WCast Size Exp
         | LitBits BV
         | LVal LVal
      deriving (Eq, Show)

expWidth :: Exp -> Maybe Size
expWidth = \ case
      Add e1 e2 -> largest e1 e2
      Sub e1 e2 -> largest e1 e2
      Mul e1 e2 -> largest e1 e2
      Div e1 e2 -> largest e1 e2
      Mod e1 e2 -> largest e1 e2

      And e1 e2 -> largest e1 e2
      Or e1 e2 -> largest e1 e2
      XOr e1 e2 -> largest e1 e2
      XNor e1 e2 -> largest e1 e2

      Pow e _    -> expWidth e
      LShift e _ -> expWidth e
      RShift e _ -> expWidth e
      LShiftArith e _ -> expWidth e
      RShiftArith e _ -> expWidth e

      Not e -> expWidth e

      LAnd _ _ -> Just 1
      LOr _ _ -> Just 1
      LNot _ -> Just 1

      RAnd _ -> Just 1
      RNAnd _ -> Just 1
      ROr _ -> Just 1
      RNor _ -> Just 1
      RXor _ -> Just 1
      RXNor _ -> Just 1

      Eq  _ _  -> Just 1
      NEq _ _  -> Just 1
      CEq _ _  -> Just 1
      CNEq _ _ -> Just 1
      Lt _ _   -> Just 1
      Gt _ _   -> Just 1
      LtEq _ _ -> Just 1
      GtEq _ _ -> Just 1
      Cond _ e1 e2 -> largest e1 e2
      Concat es  -> sumMaybes $ map expWidth es
      Repl sz e  -> (*sz) <$> expWidth e
      WCast sz _ -> pure sz
      LitBits bv -> pure $ fromIntegral $ width bv
      LVal lv    -> lvalWidth lv
      _          -> Nothing
      where largest :: Exp -> Exp -> Maybe Size
            largest e1 e2 = max <$> expWidth e1 <*> expWidth e2

ppBinOp :: Exp -> Text -> Exp -> Doc an
ppBinOp a op b = parens (pretty a) <+> text op <+> parens (pretty b)

ppUnOp :: Text -> Exp -> Doc an
ppUnOp op a = text op <> parens (pretty a)

instance Pretty Exp where
      pretty = \ case
            Add a b         -> ppBinOp a "+"   b
            Sub a b         -> ppBinOp a "-"   b
            Mul a b         -> ppBinOp a "*"   b
            Div a b         -> ppBinOp a "/"   b
            Mod a b         -> ppBinOp a "%"   b
            Pow a b         -> ppBinOp a "**"  b
            LAnd a b        -> ppBinOp a "&&"  b
            LOr a b         -> ppBinOp a "||"  b
            And a b         -> ppBinOp a "&"   b
            Or a b          -> ppBinOp a "|"   b
            XOr a b         -> ppBinOp a "^"   b
            XNor a b        -> ppBinOp a "~^"  b
            LShift a b      -> ppBinOp a "<<"  b
            RShift a b      -> ppBinOp a ">>"  b
            LShiftArith a b -> ppBinOp a "<<<" b
            RShiftArith a b -> ppBinOp a ">>>" b
            LNot a          -> ppUnOp    "!"   a
            Not a           -> ppUnOp    "~"   a
            RAnd a          -> ppUnOp    "&"   a
            RNAnd a         -> ppUnOp    "~&"  a
            ROr a           -> ppUnOp    "|"   a
            RNor a          -> ppUnOp    "~|"  a
            RXor a          -> ppUnOp    "^"   a
            RXNor a         -> ppUnOp    "~^"  a
            Eq a b          -> ppBinOp a "=="  b
            NEq a b         -> ppBinOp a "!="  b
            CEq a b         -> ppBinOp a "===" b
            CNEq a b        -> ppBinOp a "!==" b
            Lt a b          -> ppBinOp a "<"   b
            Gt a b          -> ppBinOp a ">"   b
            LtEq a b        -> ppBinOp a "<="  b
            GtEq a b        -> ppBinOp a ">="  b
            Cond e1 e2 e3   -> parens (pretty e1) <+> text "?" <+> parens (pretty e2) <+> colon <+> parens (pretty e3)
            Concat es       -> braces $ hsep $ punctuate comma $ map pretty es
            Repl i e        -> braces $ pretty i <> braces (pretty e)
            WCast sz e      -> pretty sz <> text "'" <> parens (pretty e)
            LitBits bv      -> pretty (width bv) <> text "'h" <> text (pack $ drop 2 $ showHex bv)
            LVal x          -> pretty x

bTrue :: Exp
bTrue = LitBits $ ones 1

bFalse :: Exp
bFalse = LitBits $ zeros 1

data Bit = Zero | One | X | Z
      deriving (Eq, Show)

instance Pretty Bit where
      pretty = \ case
            Zero -> text "0"
            One  -> text "1"
            X    -> text "x"
            Z    -> text "z"

data LVal = Element Name Index
          | Range Name Index Index
          | Name Name
          | LVals [LVal]
      deriving (Eq, Show)

lvalWidth :: LVal -> Maybe Size
lvalWidth = \ case
      Element _ _ -> pure 1
      Range _ i j -> pure $ fromIntegral $ j - i + 1
      Name _      -> Nothing
      LVals lvs   -> sumMaybes $ map lvalWidth lvs

sumMaybes :: Num a => [Maybe a] -> Maybe a
sumMaybes = foldMaybes (+)

foldMaybes :: (a -> a -> a) -> [Maybe a] -> Maybe a
foldMaybes f = \ case
      a : b : ms -> foldMaybes f $ (f <$> a <*> b) : ms
      [a]        -> a
      _          -> Nothing

instance Pretty LVal where
      pretty = \ case
            Element x i -> pretty x <> brackets (pretty i)
            Range x i j -> pretty x <> brackets (pretty j <> colon <> pretty i)
            Name x      -> text x
            LVals lvs   -> braces $ hsep $ punctuate comma $ map pretty lvs
