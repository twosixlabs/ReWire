module ExtensionalSemantics where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Prelude hiding ((<>), (&&), (||))

-------------
-- Streams --
-------------

infixr 0 :<

data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
  show (a0 :< _) = "[" ++ show a0 ++ " :< ...]"

nats :: Stream Int
nats = gennats 0
  where
    gennats :: Int -> Stream Int
    gennats i = i :< gennats (i + 1)

shd (a :< _) = a
stl (_ :< as) = as

takeStr :: Int -> Stream a -> [a]
takeStr 0 _ = []
takeStr n (i :< is) = i : takeStr (n-1) is

rep :: a -> Stream a
rep a = a :< rep a 

unfoldStr :: (s -> o) -> (s -> s) -> s -> Stream o
unfoldStr theta delta s = theta s :< unfoldStr theta delta (delta s)  

-------------
-- Writers --
-------------

data WriterPlus w a = w :> WriterPlus w a | w :+> a

instance (Show w , Show a) => Show (WriterPlus w a) where
  show (w :> ws) = show w ++ " :> " ++ show ws
  show (w :+> a) = show w ++ " :+> " ++ show a

lastW :: WriterPlus w a -> w
lastW (_ :> ws) = lastW ws
lastW (w :+> _) = w

---------------------
-- Monads (Fig. 4) --
---------------------

-- State monad

data ST s a = ST (s -> (a , s))

runST :: ST s a -> s -> (a , s)
runST (ST x) = x

returnS :: a -> ST s a
returnS a   = ST (\ s -> (a , s))

(*=) :: ST s a -> (a -> ST s b) -> ST s b
(ST x) *= f = ST (\ s -> let (a , s') = x s in runST (f a) s')

get :: ST s s 
get = ST (\ s -> (s , s))

put :: s -> ST s ()
put s = ST (\ _ -> (() , s))

instance Monad (ST s) where
   return  = pure
   x >>= f = x *= f

instance Functor (ST s) where
    fmap = liftM
    
instance Applicative (ST s) where
    pure  = returnS
    (<*>) = ap

-- Reactive resumption over State (Fig. 4)

data Re i s o a = P (ST s (Either a (o , i -> Re i s o a)))

deP :: Re i s o a -> ST s (Either a (o, i -> Re i s o a))
deP (P x) = x

unroll :: Re i s o a -> i -> s -> Either (a, i, s) (Re i s o a, s , o)
unroll (P x) i' s = case runST x s of
  (Left a , si)         -> Left (a , i' , si)    -- si = intermediate store
  (Right (o' , k) , s') -> Right (k i', s' , o')

returnR :: a -> Re i s o a
returnR a = P (returnS (Left a))

(>>>=) :: Re i s o a -> (a -> Re i s o b) -> Re i s o b
(P x) >>>= f = P (x *= \ r -> case r of
                                   Left a        -> deP (f a)
                                   Right (o , k) -> returnS (Right (o , \ i -> k i >>>= f)))

(<>) :: (a -> Re i s o b) -> (b -> Re i s o c) -> (a -> Re i s o c)
f <> g = \ a -> f a >>>= g

liftR :: ST s a -> Re i s o a
liftR x = P (x *= (returnS . Left))

signal :: o -> Re i s o i
signal o = P (returnS (Right (o , returnR)))

instance Monad (Re i s o) where
   return  = pure
   x >>= f = x >>>= f

instance Functor (Re i s o) where
    fmap = liftM
    
instance Applicative (Re i s o) where
    pure  = returnR
    (<*>) = ap

----------------------------------
-- Domain Semantics (Fig. 6(a)) --
----------------------------------

type DomRePlus i s o a = (i , s , o) -> Stream i -> WriterPlus (i, s, o) (a , s , Stream i) 
--                                                                     N.b., ^^^^ differs from Coq/Agda
--                                                                     Does it matter?
type DomReInf i s o    = (i , s , o) -> Stream i -> Stream (i, s, o) 

re_plus :: Re i s o a -> DomRePlus i s o a
re_plus (P phi) w@(_ , s , _) is@(i' :< is') = case runST phi s of
          (Left a , s')         -> w :+> (a , s' , is)
          (Right (o' , k) , s') -> w :> re_plus (k i') (i' , s' , o') is'

re_inf :: Re i s o a -> DomReInf i s o
re_inf (P phi) w@(_ , s , _) is@(i' :< is') = case runST phi s of
          (Left a , s')         -> error "impossible case if phi never terminates"
          (Right (o' , k) , s') -> w :< re_inf (k i') (i' , s' , o') is'


type Store i s o a = ((i , s , o) , Stream i , a -> Re i s o a)

update :: (a -> Re i s o a) -> Store i s o a -> Store i s o a
update f = (\ (w0 , is0 , acc) -> (w0 , is0 , acc <> f))

observe :: a -> Store i s o a -> (i, s, o)
observe a (w0 , is0 , acc) = lastW (re_plus (acc a) w0 is0)

fixRe :: (a -> Re i s o a) -> a -> DomReInf i s o
fixRe f a w0 is0 = unfoldStr (observe a) (update f) (w0 , is0 , f)
