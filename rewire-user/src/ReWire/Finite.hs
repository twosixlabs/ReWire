{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module ReWire.Finite where

import ReWire
import qualified ReWire.Bits as B

-- | Convert an Integer into a @'Finite' n@, throws an error if >= @n@.
{-# INLINE finite #-}
finite :: KnownNat n => Integer -> Finite n
finite = rwPrimFinite

-- | Converts argument bitvector to Finite, raising error if unrepresentable.
{-# INLINE toFinite #-}
toFinite :: KnownNat n => W m -> Finite n
toFinite = rwPrimToFinite

{-# INLINE minBound #-}
minBound :: KnownNat n => Finite n
minBound = rwPrimFiniteMinBound

{-# INLINE maxBound #-}
maxBound :: KnownNat n => Finite n
maxBound = rwPrimFiniteMaxBound

-- | Converts argument bitvector to Finite n, reducing modulo n if necessary
--   (without raising error).
{-# INLINE toFinite' #-}
toFinite' :: KnownNat n => W m -> Finite n
toFinite' = rwPrimToFiniteMod

{-# INLINE fromFinite #-}
fromFinite :: KnownNat m => Finite n -> W m
fromFinite = rwPrimFromFinite

{-# INLINE (+) #-}
(+) :: KnownNat n => Finite n -> Finite n -> Finite n
a + b = toFinite' $ (fromFinite a :: B.Lit) B.+ fromFinite b

{-# INLINE (-) #-}
(-) :: KnownNat n => Finite n -> Finite n -> Finite n
a - b = toFinite' $ (fromFinite a :: B.Lit) B.- fromFinite b

{-# INLINE (*) #-}
(*) :: KnownNat n => Finite n -> Finite n -> Finite n
a * b = toFinite' $ (fromFinite a :: B.Lit) B.* fromFinite b

{-# INLINE div #-}
div :: KnownNat n => Finite n -> Finite n -> Finite n
div a b = toFinite' $ (fromFinite a :: B.Lit) B./ fromFinite b

{-# INLINE (==) #-}
(==) :: Finite n -> Finite n -> Bool
a == b = (fromFinite a :: B.Lit) B.== (fromFinite b :: B.Lit)

{-# INLINE (<) #-}
(<) :: Finite n -> Finite n -> Bool
a < b = (fromFinite a :: B.Lit) B.< (fromFinite b :: B.Lit)

{-# INLINE even #-}
even :: Finite n -> Bool
even a = B.even (fromFinite a :: B.Lit)

{-# INLINE odd #-}
odd :: Finite n -> Bool
odd a = B.odd (fromFinite a :: B.Lit)
