{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ReWire.Interactive where
--         ( runStateT , runReacT , binary ) where

-- import Prelude hiding (head, (<>), (==), (-), (^), (&&), (||))

import qualified Control.Monad.Identity as GHC
import qualified Control.Monad.Resumption.Reactive as GHC
import qualified Control.Monad.State as GHC
import qualified Data.Vector.Sized as V

type Vec n a = V.Vector n a
-- type Bit = Bool

class ShowHex w where
  xshow :: w -> String

class ShowBin w where
  bshow :: w -> String

class ShowDec w where
  dshow :: w -> String

instance ShowDec (Vec n Bool) where
  dshow = show . fromBits
   where
     fromBits :: Vec n Bool -> Int
     fromBits v = gorf 0 0 (reverse (V.toList v))

     gorf :: Int -> Int -> [Bool] -> Int
     gorf _ acc []           = acc
     gorf n acc (True : bs)  = gorf (n+1) (2 Prelude.^ n + acc) bs
     gorf n acc (False : bs) = gorf (n+1) acc bs

instance ShowBin (Vec n Bool) where
  bshow bs = "0b" ++ (map bool2bit (V.toList bs))
   where
     bool2bit :: Bool -> Char
     bool2bit True  = '1'
     bool2bit False = '0'

instance ShowHex (Vec n Bool) where
  xshow bs = {- "0x" ++ -} hexify (V.toList bs)
   where
     hexify :: [Bool] -> String
     hexify bits = map toHex (reverse (fours False (reverse bits)))
     fours :: a -> [a] -> [(a,a,a,a)]
     fours _ []                       = []
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
     toHex (True , False , True , False)   = 'A'
     toHex (True , False , True , True)    = 'B'
     toHex (True , True , False , False)   = 'C'
     toHex (True , True , False , True)    = 'D'
     toHex (True , True , True , False)    = 'E'
     toHex (True , True , True , True)     = 'F'

binary :: Vec n Bool -> IO ()
binary = putStrLn . bshow

hex :: Vec n Bool -> IO ()
hex = putStrLn . xshow

dec :: Vec n Bool -> IO ()
dec = putStrLn . dshow

{- -}

runIdentity :: GHC.Identity a -> a
runIdentity (GHC.Identity x) = x

runStateT :: GHC.StateT s m a -> s -> m (a , s)
runStateT (GHC.StateT x) = x

runReacT :: GHC.ReacT i o m a -> m (Either a (o , i -> GHC.ReacT i o m a))
runReacT (GHC.ReacT x) = x

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

instance Pretty () where
  pp () = "()"

instance Pretty Bool where
  pp = show

instance Pretty a => Pretty (Maybe a) where
  pp Nothing = "Nothing"
  pp (Just a) = "Just " ++ pp a

instance Pretty a => Pretty [a] where
  pp []       = "[]"
  pp (x : xs) = pp x ++ " : " ++ pp xs

instance (Pretty w , Pretty a) => Pretty (WriterPlus w a) where
  pp (w :> ws) = pp w ++ " :> " ++ pp ws
  pp (w :+> a) = pp w ++ " :+> " ++ pp a

type Re i s o a   = GHC.ReacT i o (GHC.StateT s GHC.Identity) a
type RePure i o a = GHC.ReacT i o GHC.Identity a

step :: Re i s o a -> i -> s -> Either (a , s) (Re i s o a , s , o)
step (GHC.ReacT x) i' s = case GHC.runIdentity (runStateT  x s) of
                             (Left a , si)         -> Left (a , si)
                             (Right (o' , k) , s') -> Right (k i' , s' , o')

stepPure :: RePure i o a -> i -> Either a (RePure i o a , o)
stepPure (GHC.ReacT x) i' = case GHC.runIdentity x of
                                 (Left a)         -> Left a
                                 (Right (o' , k)) -> Right (k i' , o')

iterRe :: (i -> Re i s o a) -> i -> s -> Either (a, s) (i -> Re i s o a, s, o)
iterRe f i' s = case GHC.runIdentity (runStateT (runReacT (f i')) s) of
                     (Left a , si)         -> Left (a , si)
                     (Right (o' , k) , s') -> Right (k  , s' , o')

grunt :: (i -> Re i s o a) -> (i , s , o) -> [i] -> WriterPlus (i , s , o) (Maybe (a , i , s))
grunt _ (i0 , s0 , o0) []      = (i0 , s0 , o0) :+> Nothing
grunt f (i0, s0, o0) (i' : is) = case iterRe f i0 s0 of
                                       Left (a , si)       -> (i0 , s0 , o0) :+> Just (a , i' , si)
                                       Right (k , s' , o') -> (i0 , s0 , o0) :> grunt k (i' , s' , o') is


run :: Re i s o a -> (i , s , o) -> [i] -> WriterPlus (i , s , o) (Maybe (a , i , s))
run _ (i0, s0, o0) []        = (i0 , s0 , o0) :+> Nothing
run x (i0, s0, o0) (i' : is) = case step x i' s0 of
                                       Left (a , si)        -> (i0 , s0 , o0) :+> Just (a , i' , si)
                                       Right (x' , s' , o') -> (i0 , s0 , o0) :> run x' (i' , s' , o') is


runP :: RePure i o a -> (i , o) -> [i] -> WriterPlus (i , o) (Maybe (a , i))
runP _ (i0, o0) []        = (i0 , o0) :+> Nothing
runP x (i0, o0) (i' : is) = case stepPure x i' of
                                 Left a          -> (i0 , o0) :+> Just (a , i')
                                 Right (x' , o') -> (i0 , o0) :> runP x' (i' , o') is


data WriterPlus w a = w :> WriterPlus w a | w :+> a

instance (Show w , Show a) => Show (WriterPlus w a) where
  show (w :> ws) = show w ++ " :> " ++ show ws
  show (w :+> a) = show w ++ " :+> " ++ show a
