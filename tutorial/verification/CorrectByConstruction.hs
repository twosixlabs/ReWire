{-# LANGUAGE DataKinds #-}
module CorrectByConstruction where

import Prelude hiding ((^),(+))
import ReWire.Bits
import ReWire hiding (signal, get, put)

import ExtensionalSemantics
import qualified Data.Vector.Sized as V
-- import ReWire.Interactive

-- | RWC will complain in this is imported
-- import ReWire.Interactive 

type W8 = W 8

f :: KnownNat n => W n -> W n -> W n -> (W n , W n)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )

-- | this is just f.
purecsa :: KnownNat n => W n -> W n -> W n -> (W n, W n)
purecsa a b c = 
  let anb  = a .&. b in
  let anc  = a .&. c in
  let bnc  = b .&. c in
  let tmp1 = anb .|. anb .|. bnc <<. lit 1 in
  let tmp2 = (a ^ b) ^ c
  in
      (tmp1 , tmp2)

-- |
-- | This tweeks PCSA to store intermediate values.
-- |

data RegFile = RegFile { ra , rb , rc , a_and_b , a_and_c , b_and_c :: W 8 }
data Reg     = RA | RB | RC | A_and_B | A_and_C | B_and_C
data Inp a   = DontCare | Arg0 a | Arg1 a | Arg2 a | Start 

data In a    = DC | Args a | Go

-- type M = StateT RegFile Identity
-- type Dev = ReacT (Inp (W 8)) (Maybe (W 8, W 8)) (StateT RegFile Identity)
type M   = ST RegFile
type Dev i s o = Re i s o

trReg :: Reg -> RegFile -> W 8
trReg r = case r of       
               RA      -> ra
               RB      -> rb
               RC      -> rc
               A_and_B -> a_and_b
               A_and_C -> a_and_c
               B_and_C -> b_and_c

readReg :: Reg -> M (W 8)
readReg v = do
             rf <- get
             return (trReg v rf)

setReg :: Reg -> W 8 -> M ()
setReg r w = do
               rf <- get
               case r of
                 ----
                 RA      -> put $ rf { ra = w }
                 RB      -> put $ rf { rb = w }
                 RC      -> put $ rf { rc = w } 
                 A_and_B -> put $ rf { a_and_b = w } 
                 A_and_C -> put $ rf { a_and_c = w }
                 B_and_C -> put $ rf { b_and_c = w } 
               
regfile0 :: RegFile
regfile0 = RegFile { ra      = lit 0
                   , rb      = lit 0
                   , rc      = lit 0
                   , a_and_b = lit 0
                   , a_and_c = lit 0
                   , b_and_c = lit 0
                   }

regfile1 :: RegFile
regfile1 = RegFile { ra      = lit 1
                   , rb      = lit 2
                   , rc      = lit 3
                   , a_and_b = lit 4
                   , a_and_c = lit 5
                   , b_and_c = lit 6
                   }

--
-- This recasts purecsa in imperative form.
--

-- impcsa :: W 8 -> W 8 -> W 8 -> StateT RegFile Identity (W 8, W 8)
impcsa :: W 8 -> W 8 -> W 8 -> M (W 8, W 8)
impcsa a b c = do
  setReg RA a
  setReg RB b
  setReg RC c
  readReg RA >>= \ a -> readReg RB >>= \ b -> setReg A_and_B (a .&. b)
  readReg RA >>= \ a -> readReg RC >>= \ c -> setReg A_and_C (a .&. c)
  readReg RB >>= \ b -> readReg RC >>= \ c -> setReg B_and_C (b .&. c)
  tmp1 <- do
            anb <- readReg A_and_B
            anc <- readReg A_and_C
            bnc <- readReg B_and_C
            return (anb .|. anb .|. bnc <<. lit 1)
  tmp2 <- do
            a <- readReg RA
            b <- readReg RB
            c <- readReg RC
            return ((a ^ b) ^ c)
  return (tmp1 , tmp2)

--
-- Three staging operators
-- 

{-# INLINE cycN #-}
-- cycN :: Monad m => m a -> ReacT i (Maybe o) m a
cycN :: ST s a -> Dev i s (Maybe o) a
cycN x         = do
                   v <- liftR x
                   signal Nothing
                   return v

{-# INLINE cycN_ #-}
-- cycN_ :: Monad m => m a -> ReacT i (Maybe o) m i
cycN_ :: ST s a -> Dev i s (Maybe o) i
cycN_ x        = do
                   liftR x
                   i' <- signal Nothing
                   return i' 


{-# INLINE _cycN #-}
-- _cycN :: Monad m => m a -> ReacT i (Maybe o) m ()
_cycN :: ST s () -> Dev i s (Maybe o) ()
_cycN x        = do
                   liftR x
                   signal Nothing
                   return ()

simple :: (W8, W8, W8) -> Re (W8, W8, W8) () (W8, W8) ()
simple (a , b , c) = signal (f a b c) >>= simple

gosimp x n = putStrLn $ pp $ takeStr n (re_inf x w0 ins)
   where
     w0 :: ( (W8, W8, W8) , () , (W8, W8) )
     w0 = ( (lit 0 , lit 0 , lit 0) , () , (lit 0 , lit 0) )
     ins :: Stream (W8, W8, W8)
     ins = (lit 40 , lit 25 , lit 20) :< (lit 41 , lit 25 , lit 20) :< ins
     
-- doit :: Re (Inp (W 8)) RegFile (Maybe (W 8, W 8)) a -> Int -> IO ()
-- doit x n = putStrLn $ pp $ takeStr n (re_inf x snapshot1 ins)


--altcsa :: W 8 -> W 8 -> W 8 -> M (W 8, W 8)
-- altcsa = do
--   a <- signal Nothing
--   liftR $ setReg RA a
--   b <- signal Nothing
--   liftR $ setReg RB b
--   c <- signal Nothing
--   liftR $ setReg RC c
  
--   readReg RA >>= \ a -> readReg RB >>= \ b -> setReg A_and_B (a .&. b)
--   readReg RA >>= \ a -> readReg RC >>= \ c -> setReg A_and_C (a .&. c)
--   readReg RB >>= \ b -> readReg RC >>= \ c -> setReg B_and_C (b .&. c)
--   tmp1 <- do
--             anb <- readReg A_and_B
--             anc <- readReg A_and_C
--             bnc <- readReg B_and_C
--             return (anb .|. anb .|. bnc <<. lit 1)
--   tmp2 <- do
--             a <- readReg RA
--             b <- readReg RB
--             c <- readReg RC
--             return ((a ^ b) ^ c)
--   return (tmp1 , tmp2)


--
-- CSA in ReWire.
-- Here, the arguments are passed in interactively to the device, one argument per cycle.
--

csa :: In (W 8, W 8, W 8) -> Re (In (W 8, W 8, W 8)) RegFile (Maybe (W 8, W 8)) ()
csa DC             = signal Nothing >>= csa
csa (Args (a,b,c)) = (cycN_ $ setReg RA a >> setReg RB b >> setReg RC c) >>= csa
csa Go             = do
                       v <- liftR $ do
                                      a <- readReg RA
                                      b <- readReg RB
                                      c <- readReg RC
                                      let anb = a .&. b
                                      let anc = a .&. c
                                      let bnc = b .&. c
                                      return (anb .|. anb .|. bnc <<. lit 1 , (a ^ b) ^ c)
                       i <- signal (Just v)
                       csa i

rwcsa :: Inp (W 8) -> Dev (Inp (W 8)) RegFile (Maybe (W 8, W 8)) ()
rwcsa DontCare = signal Nothing >>= rwcsa
rwcsa (Arg0 a) = do
                   i <- cycN_ $ setReg RA a
                   rwcsa i
rwcsa (Arg1 b) = do
                   i <- cycN_ $ setReg RB b
                   rwcsa i
rwcsa (Arg2 c) = do
                   i <- cycN_ $ setReg RC c
                   rwcsa i
rwcsa Start    = do
                   cycN_ $ readReg RA >>= \ a -> readReg RB >>= \ b -> setReg A_and_B (a .&. b)
                   cycN_ $ readReg RA >>= \ a -> readReg RC >>= \ c -> setReg A_and_C (a .&. c)
                   cycN_ $ readReg RB >>= \ b -> readReg RC >>= \ c -> setReg B_and_C (b .&. c)
                   tmp1 <- cycN $ do
                             anb <- readReg A_and_B
                             anc <- readReg A_and_C
                             bnc <- readReg B_and_C
                             return (anb .|. anb .|. bnc <<. lit 1)
                   tmp2 <- cycN $ do
                             a <- readReg RA
                             b <- readReg RB
                             c <- readReg RC
                             return ((a ^ b) ^ c)
                   i <- signal (Just (tmp1 , tmp2))
                   rwcsa i


-- ex1

ins :: Stream (Inp (W 8))
ins = Arg0 a40 :< Arg1 a25 :< Arg2 a20 :< Start :< dcs
  where
    dcs = DontCare :< dcs

snapshot0 , snapshot1 :: (Inp (W 8), RegFile, Maybe (W 8, W 8))
snapshot0 = (Arg0 a40 , regfile0 , Nothing)
snapshot1 = (DontCare , regfile0 , Nothing)
snapshot2 = (Arg0 a7 , regfile1 , Nothing)
  
-- ex2
s0 = (DC , regfile0 , Nothing)

is :: Stream (In (W 8 , W 8 , W8))
is = Args (a40 , a25 , a20) :< Go :< dcs
  where
    dcs = DC :< dcs

{-
start :: ReacT (Inp (W 8)) (Maybe (W 8, W 8)) Identity ()
start = extrude (signal Nothing >>= rwcsa) regfile0
-}


go :: Pretty a => Re (Inp (W 8)) RegFile (Maybe (W 8 , W 8)) a -> IO ()
--          WriterPlus (Inp (W 8), RegFile, Maybe (W 8 , W 8)) (a, RegFile, Stream (Inp (W 8)))
go x = putStrLn $ pp $ re_plus x snapshot1 ins

doit :: Re (Inp (W 8)) RegFile (Maybe (W 8, W 8)) a -> Int -> IO ()
doit x n = putStrLn $ pp $ takeStr n (re_inf x snapshot1 ins)

fun1 :: Pretty a => Int -> Stream a -> IO ()
fun1 = curry $ putStrLn . pp . uncurry takeStr

-- go2 x n = fun1 n (re_inf x snap0 is)

ex1 = signal Nothing >>= rwcsa
ex2 = signal Nothing >>= csa

-- Definition snap0 : nat * unit * option nat := (0 , tt , None).
snap0 :: (Int, Int, Maybe Int)
snap0 = (99 , 777 , Nothing)

e0 , e1 , e2 , e4 :: WriterPlus (Int, Int, Maybe Int) (Int, Int, Stream Int)
e3 :: WriterPlus (Int, Int, Maybe Int) ((), Int, Stream Int)
e0 = re_plus (signal (Just 101)) snap0 nats
e1 = re_plus (liftR (put 888 >> return 9)) snap0 nats
e2 = re_plus (liftR (put 888 >> return 9) >> signal (Just 101)) snap0 nats
e3 = re_plus (liftR (put 888 >> return 9) >> signal (Just 101) >> (liftR (put 999))) snap0 nats
e4 = re_plus (liftR (put 888 >> return 9) >> signal (Just 101) >>= \ i' -> (liftR (put 999) >> signal (Just i'))) snap0 nats

{-
ghci> e0
(99,777,Nothing) :> (0,777,Just 101) :+> (0,777,[1 :< ...])
ghci> e1
(99,777,Nothing) :+> (9,888,[0 :< ...])
ghci> e2
(99,777,Nothing) :> (0,888,Just 101) :+> (0,888,[1 :< ...])
ghci> e3
(99,777,Nothing) :> (0,888,Just 101) :+> ((),999,[1 :< ...])
ghci> e4
(99,777,Nothing) :> (0,888,Just 101) :> (1,999,Just 0) :+> (1,999,[2 :< ...])
-}
  
fubar :: WriterPlus (Inp (W 8), RegFile, Maybe (W 8, W 8)) (Inp (W 8), RegFile, Stream (Inp (W 8)))
fubar = (re_plus (cycN_ $ setReg RA (lit 0xFF)) snapshot1 ins)
{-
ghci> pretty fubar
(Arg0 0x07,(0x00 , 0x00 , 0x00 , 0x00 , 0x00 , 0x00),Nothing) :>
(Arg0 0x28,(0xff , 0x00 , 0x00 , 0x00 , 0x00 , 0x00),Nothing) :+>
  (Arg0 0x28,(0xff , 0x00 , 0x00 , 0x00 , 0x00 , 0x00),(Arg1 0x19 :< ...))
-}

three = do
          cycN_ $ readReg RA >>= \ a -> readReg RB >>= \ b -> setReg A_and_B (a + b)
          cycN_ $ readReg RA >>= \ a -> readReg RC >>= \ c -> setReg A_and_C (a + c)
          cycN_ $ readReg RB >>= \ b -> readReg RC >>= \ c -> setReg B_and_C (b + c)
{-
ghci> go three
(Arg0 0x07,(0x01 , 0x02 , 0x03 , 0x04 , 0x05 , 0x06),Nothing) :>
(Arg0 0x28,(0x01 , 0x02 , 0x03 , 0x03 , 0x05 , 0x06),Nothing) :>
(Arg1 0x19,(0x01 , 0x02 , 0x03 , 0x03 , 0x04 , 0x06),Nothing) :>
(Arg2 0x14,(0x01 , 0x02 , 0x03 , 0x03 , 0x04 , 0x05),Nothing) :+>
(Arg2 0x14,(0x01 , 0x02 , 0x03 , 0x03 , 0x04 , 0x05),(Start :< ...))"
-}

--
-- What's below is from Interactive.
-- 


a7 , a40 , a25 , a20 :: W 8
a40 = lit 40
a25 = lit 25
a20 = lit 20
a7  = lit 7

class ShowBin w where
  bshow :: w -> String

instance ShowBin (Vec n Bool) where
  bshow bs = "0b" ++ (map bool2bit (V.toList bs))
   where
     bool2bit :: Bool -> Char
     bool2bit True  = '1'
     bool2bit False = '0'

class ShowHex w where
  xshow :: w -> String
instance ShowHex (Vec n Bool) where
  xshow bs = "0x" ++ hexify (V.toList bs)
   where
     hexify :: [Bool] -> String
     hexify bits = map toHex (reverse (fours False (reverse bits)))
     fours :: a -> [a] -> [(a,a,a,a)]
     fours d []                       = []
     fours d (x1 : x2 : x3 : x4 : xs) = (x4 , x3 , x2 , x1) : fours d xs
     fours d (x1 : x2 : x3 : [])      = (d , x3 , x2 , x1) : []
     fours d (x1 : x2 : [])           = (d , d , x2 , x1) : []
     fours d (x1 : [])                = (d , d , d , x1) : []

     toHex :: (Bool , Bool , Bool , Bool) -> Char
     toHex (False , False , False , False) = '0'
     toHex (False , False , False , True)  = '1'
     toHex (False , False , True , False)  = '2'
     toHex (False , False , True , True)   = '3'
     toHex (False , True , False , False)  = '4'
     toHex (False , True , False , True)   = '5'
     toHex (False , True , True , False)   = '6'
     toHex (False , True , True , True)    = '7'
     toHex (True , False , False , False)  = '8'
     toHex (True , False , False , True)   = '9'
     toHex (True , False , True , False)   = 'a'
     toHex (True , False , True , True)    = 'b'
     toHex (True , True , False , False)   = 'c'
     toHex (True , True , False , True)    = 'd'
     toHex (True , True , True , False)    = 'e'
     toHex (True , True , True , True)     = 'f'

newtype T8 a  = T8 (a , a , a , a , a , a , a , a )

instance ShowHex a => Show (T8 a) where
  show (T8 (a0 , a1 , a2 , a3 , a4 , a5 , a6 , a7 )) = "(" Prelude.++ xshow a0 Prelude.++ " , " Prelude.++
                                                              xshow a1 Prelude.++ " , " Prelude.++
                                                              xshow a2 Prelude.++ " , " Prelude.++
                                                              xshow a3 Prelude.++ " , " Prelude.++
                                                              xshow a4 Prelude.++ " , " Prelude.++
                                                              xshow a5 Prelude.++ " , " Prelude.++
                                                              xshow a6 Prelude.++ " , " Prelude.++
                                                              xshow a7 Prelude.++ ")"

hex :: Vec n Bool -> IO () 
hex = putStrLn . xshow

pretty :: Pretty a => a -> IO ()
pretty a = putStrLn $ pp a

class Pretty a where
  pp :: a -> String

instance Pretty (V.Vector n Bool) where
  pp bs = xshow bs

instance (Pretty a, Pretty b) => Pretty (a , b) where
  pp (a , b) = "(" ++ pp a ++ "," ++ pp b ++ ")"

instance (Pretty a, Pretty b , Pretty c) => Pretty (a , b , c) where
  pp (a , b , c) = "(" ++ pp a ++ "," ++ pp b ++ "," ++ pp c ++ ")"

instance Pretty a => Pretty (Stream a) where
  pp (a :< _) = "(" ++ pp a ++ " :< ..." ++ ")"

instance Pretty () where
  pp () = "()"

instance Pretty Bool where
  pp = show

instance Pretty a => Pretty (Maybe a) where
  pp Nothing = "Nothing"
  pp (Just a) = "Just " ++ pp a

instance Pretty a => Pretty [a] where
  pp []       = "[]"
  pp (x : xs) = pp x ++ " : \n" ++ pp xs

instance (Pretty w , Pretty a) => Pretty (WriterPlus w a) where
  pp (w :> ws) = pp w ++ " :> " ++ pp ws
  pp (w :+> a) = pp w ++ " :+> " ++ pp a

instance Pretty a => Pretty (Inp a) where
  pp (Arg0 x) = "Arg0 " ++ pp x
  pp (Arg1 x) = "Arg1 " ++ pp x
  pp (Arg2 x) = "Arg2 " ++ pp x
  pp Start    = "Start"
  pp DontCare = "DontCare"

instance Pretty a => Pretty (In a) where
  pp (Args x) = "Args " ++ pp x
  pp Go       = "Go"
  pp DC       = "DC"

-- data RegFile = RegFile { ra , rb , rc , a_and_b , a_and_c , b_and_c :: W 8 }

instance Pretty RegFile where
  pp (RegFile { ra , rb , rc , a_and_b , a_and_c , b_and_c }) = "(" ++ xshow ra ++ " , "
                                                                    ++ xshow rb ++ " , " 
                                                                    ++ xshow rc ++ " , " 
                                                                    ++ xshow a_and_b ++ " , " 
                                                                    ++ xshow a_and_c ++ " , " 
                                                                    ++ xshow b_and_c 
                                                                    ++ ")"

{-
cycN_ $ setReg RA (lit 10)

"(Arg0 0x28,(0x00 , 0x00 , 0x00 , 0x00 , 0x00 , 0x00),Nothing) :
(Arg0 0x28,(0x28 , 0x00 , 0x00 , 0x00 , 0x00 , 0x00),Nothing) :
(Arg1 0x19,(0x28 , 0x00 , 0x00 , 0x00 , 0x00 , 0x00),Nothing) :
(Arg2 0x14,(0x28 , 0x19 , 0x00 , 0x00 , 0x00 , 0x00),Nothing) :
(Start,(0x28 , 0x19 , 0x14 , 0x00 , 0x00 , 0x00),Nothing) :
(Start,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x00),Nothing) :
(Start,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x00),Nothing) :
(Start,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) :
(Start,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) :
(Start,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) :
(DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Just (0x28,0x25)) :
(DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : (DontCare,(0x28 , 0x19 , 0x14 , 0x08 , 0x00 , 0x10),Nothing) : []"
-}

{-
body :: Inp (W 8) -> (Inp (W 8) -> Dev (Inp (W 8)) RegFile (Maybe (W 8, W 8)) ()) -> Dev (Inp (W 8)) RegFile (Maybe (W 8, W 8)) ()
body DontCare k = signal Nothing >>= k
body (Arg0 a) k = do
                     i <- cycN_ $ setReg RA a
                     k i
body (Arg1 b) k = do
                     i <- cycN_ $ setReg RB b
                     k i
body (Arg2 c) k = do
                     i <- cycN_ $ setReg RC c
                     k i
body Start k    = do
                     cycN_ $ readReg RA >>= \ a -> readReg RB >>= \ b -> setReg A_and_B (a .&. b)
                     cycN_ $ readReg RA >>= \ a -> readReg RC >>= \ c -> setReg A_and_C (a .&. c)
                     cycN_ $ readReg RB >>= \ b -> readReg RC >>= \ c -> setReg B_and_C (b .&. c)
                     tmp1 <- cycN $ do
                             anb <- readReg A_and_B
                             anc <- readReg A_and_C
                             bnc <- readReg B_and_C
                             return (anb .|. anb .|. bnc <<. lit 1)
                     tmp2 <- cycN $ do
                             a <- readReg RA
                             b <- readReg RB
                             c <- readReg RC
                             return ((a ^ b) ^ c)
                     i <- signal (Just (tmp1 , tmp2))
                     k i
-}
