{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module ReWire
      ( module RWC.Primitives
      , error, externWithSig, extern
      , setRef, getRef, put, get
      , signal, lift, extrude, unfold
      , fromList, replicate, reverse
      , index, (++)
      , slice, rslice
      , modify
      , empty, singleton, cons, snoc, head, last, length
      , take, init, drop, tail
      ) where

import RWC.Primitives

import Prelude (String, Integer)

{-# INLINE error #-}
error :: String -> a
error = rwPrimError

{-# INLINE externWithSig #-}
externWithSig :: [(String, Integer)] -- ^ Module parameters (name and integer literal value).
              -> String              -- ^ Clock signal name or empty for no clock.
              -> [(String, Integer)] -- ^ Module inputs (name and integer literal bitwidth).
              -> [(String, Integer)] -- ^ Module outputs (name and integer literal bitwidth).
              -> String              -- ^ Module name.
              -> a                   -- ^ Haskell definition to use when interpreting.
              -> String              -- ^ Instance name to use in generated Verilog.
              -> a
externWithSig = rwPrimExtern

-- | The String argument must be a string literal (after inlining).
{-# INLINE extern #-}
extern :: String -> a -> a
extern n a = externWithSig [] "" [] [] n a ""

{-# INLINE setRef #-}
setRef :: Ref a -> a -> b -> b
setRef = rwPrimSetRef

{-# INLINE getRef #-}
getRef :: Ref a -> a
getRef = rwPrimGetRef

{-# INLINE put #-}
put :: Monad m => s -> StateT s m ()
put = rwPrimPut

{-# INLINE get #-}
get :: Monad m => StateT s m s
get = rwPrimGet

{-# INLINE signal #-}
signal :: Monad m => o -> ReacT i o m i
signal = rwPrimSignal

{-# INLINE lift #-}
lift :: (MonadTrans t, Monad m) => m a -> t m a
lift = rwPrimLift

{-# INLINE extrude #-}
extrude :: ReacT i o (StateT s m) a -> s -> ReacT i o m a
extrude = rwPrimExtrude

{-# INLINE unfold #-}
unfold :: ((R_, s) -> i -> PuRe s o) -> PuRe s o -> ReacT i o Identity a
unfold = rwPrimUnfold

{-# INLINE fromList #-}
fromList :: [a] -> Vec n a
fromList = rwPrimVecFromList

{-# INLINE replicate #-}
replicate :: a -> Vec n a
replicate = rwPrimVecReplicate

{-# INLINE reverse #-}
reverse :: Vec n a -> Vec n a
reverse = rwPrimVecReverse

{-# INLINE slice #-}
slice :: Proxy i -> Vec ((i + n) + m) a -> Vec n a
slice = rwPrimVecSlice

{-# INLINE rslice #-}
rslice :: Proxy i -> Vec ((i + n) + m) a -> Vec n a
rslice = rwPrimVecRSlice

{-# INLINE index #-}
index :: Vec ((n + m) + 1) a -> Proxy n -> a
index = rwPrimVecIndex

{-# INLINE (++) #-}
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) = rwPrimVecConcat

{-# INLINE empty #-}
empty :: Vec 0 a
empty = fromList []

{-# INLINE singleton #-}
singleton :: a -> Vec 1 a
singleton a = fromList [a]

{-# INLINE cons #-}
cons :: a -> Vec n a -> Vec (1 + n) a
cons a v = fromList [a] ++ v

{-# INLINE snoc #-}
snoc :: Vec n a -> a -> Vec (n + 1) a
snoc v a = v ++ fromList [a]

head :: Vec (1 + n) a -> a
head v = index v (Proxy :: Proxy 0)

last :: Vec (n + 1) a -> a
last v = index v (lastIndex v)

lastIndex :: Vec (n + 1) a -> Proxy n
lastIndex _ = Proxy

length :: Vec n a -> Proxy n
length _ = Proxy

take :: Vec (n + m) a -> Vec n a
take v = slice (Proxy :: Proxy 0) v

init :: Vec (n + 1) a -> Vec n a
init v = take v

drop :: Vec (n + m) a -> Vec m a
drop v = rslice (Proxy :: Proxy 0) v

tail :: Vec (1 + n) a -> Vec n a
tail v = drop v

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> StateT s m ()
modify f = get `rwPrimBind` (\ x -> put (f x))
