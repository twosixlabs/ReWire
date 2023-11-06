{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module ReWire
      ( module RWC.Primitives
      , error, externWithSig, extern
      , setRef, getRef, put, get, modify
      , signal, lift, extrude, unfold
      , natVal, length, len, fromList
      , Bit, W
      ) where

import RWC.Primitives
import Prelude (String, Integer, Bool)

type Bit = Bool
type W n = Vec n Bit

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

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> StateT s m ()
modify f = get `rwPrimBind` (\ x -> put (f x))

{-# INLINE signal #-}
signal :: Monad m => o -> ReacT i o m i
signal = rwPrimSignal

{-# INLINE lift #-}
lift :: (MonadTrans t, Monad m) => m a -> t m a
lift = rwPrimLift

{-# INLINE extrude #-}
extrude :: Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m a
extrude = rwPrimExtrude

{-# INLINE unfold #-}
unfold :: ((R_, s) -> i -> PuRe s o) -> PuRe s o -> ReacT i o Identity A_
unfold = rwPrimUnfold

-- | Produce integer associated with type-level natural.
{-# INLINE natVal #-}
natVal :: KnownNat n => Proxy n -> Integer
natVal = rwPrimNatVal

{-# INLINE length #-}
length :: Vec n a -> Proxy n
length _ = Proxy

{-# INLINE len #-}
len :: KnownNat n => Vec n a -> Integer
len v = natVal (length v)

{-# INLINE fromList #-}
fromList :: KnownNat n => [a] -> Vec n a
fromList = rwPrimVecFromList
