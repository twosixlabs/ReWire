{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module RWC.Primitives
      ( Identity, ReacT, A_, R_, StateT, Vec, Finite, PuRe, Ref (..), Proxy (..)
      , rwPrimAdd
      , rwPrimAnd
      , rwPrimBind
      , rwPrimBitIndex
      , rwPrimBitSlice
      , rwPrimBits
      , rwPrimDiv
      , rwPrimEq
      , rwPrimError
      , rwPrimExtern
      , rwPrimExtrude
      , rwPrimGet
      , rwPrimGetRef
      , rwPrimGt
      , rwPrimGtEq
      , rwPrimLAnd
      , rwPrimLNot
      , rwPrimLOr
      , rwPrimLShift
      , rwPrimLift
      , rwPrimLt
      , rwPrimLtEq
      , rwPrimMSBit
      , rwPrimMod
      , rwPrimMul
      , rwPrimNatVal
      , rwPrimNot
      , rwPrimOr
      , rwPrimPow
      , rwPrimPut
      , rwPrimRAnd
      , rwPrimRNAnd
      , rwPrimRNor
      , rwPrimROr
      , rwPrimRShift
      , rwPrimRShiftArith
      , rwPrimRXNor
      , rwPrimRXOr
      , rwPrimResize
      , rwPrimReturn
      , rwPrimSetRef
      , rwPrimSignal
      , rwPrimSignextend
      , rwPrimSub
      , rwPrimUnfold
      , rwPrimFinite
      , rwPrimFiniteMinBound
      , rwPrimFiniteMaxBound
      , rwPrimToFinite
      , rwPrimToFiniteMod
      , rwPrimFromFinite
      -- , rwPrimVecBulkUpdate
      , rwPrimVecConcat
      , rwPrimVecFromList
      , rwPrimVecIndex
      , rwPrimVecIndexProxy
      , rwPrimVecMap
      , rwPrimVecGenerate
      -- , rwPrimVecIterate
      -- , rwPrimVecZip
      , rwPrimVecRSlice
      , rwPrimVecReplicate
      , rwPrimVecReverse
      , rwPrimVecSlice
      , rwPrimVecUpdate
      , rwPrimXNor
      , rwPrimXOr
      , rwPrimToInteger
      , type (+), type GHC.Monad, type GHC.MonadTrans, KnownNat
      ) where

-- Imports in this file are ignored by rwc.
import Prelude ((.),($))
import qualified Prelude                           as GHC
import qualified Control.Monad.Identity            as GHC
import qualified Control.Monad.Resumption.Reactive as GHC
import qualified Control.Monad.State               as GHC
import qualified Data.Bits                         as GHC
import GHC.TypeLits (Nat, type (+), natVal)
import qualified GHC.TypeLits                      as TL
import qualified Data.Finite                       as F
import qualified Data.Vector.Sized                 as V
import qualified Data.Vector                       as VU
import qualified ReWire.BitWord                    as BW

type Identity   = GHC.Identity
type ReacT      = GHC.ReacT
type StateT     = GHC.StateT
type Integer    = GHC.Integer
type String     = GHC.String
type Bool       = GHC.Bool
type Vec        = V.Vector
type KnownNat   = TL.KnownNat
type Finite     = F.Finite

-- ReWire primitives.

-- Primitive types:
-- data (->) a b
-- data ReacT i o m a
-- data StateT s m a
-- data Identity a
-- data Integer
-- data Bit

-- Also tuples:
-- data () = ()
-- data (a, b) = (a, b)
-- ...

data R_ -- Ctors generated during program build.
data A_ -- Ctors generated during program build.
data Proxy (n :: Nat) = Proxy
data Ref a = Ref String
data PuRe s o = Done (A_, s) | Pause (o, (R_, s))

-- Definitions in this file are for ghc compat and ignored by rwc.

rwPrimError :: String -> a
rwPrimError = GHC.error

-- | The String and list arguments must be literals (after inlining).
rwPrimExtern :: [(String, Integer)] -- ^ Module parameters (name and integer literal value).
             -> String              -- ^ Clock signal name or empty for no clock.
             -> String              -- ^ Reset signal name or empty for no reset.
             -> [(String, Integer)] -- ^ Module inputs (name and integer literal bitwidth).
             -> [(String, Integer)] -- ^ Module outputs (name and integer literal bitwidth).
             -> String              -- ^ Module name.
             -> a                   -- ^ Haskell definition to use when interpreting.
             -> String              -- ^ Instance name to use in generated Verilog.
             -> a
rwPrimExtern _ _ _ _ _ _ f _ = f

rwPrimSetRef :: Ref a -> a -> b -> b
rwPrimSetRef _ _ b = b

rwPrimGetRef :: Ref a -> a
rwPrimGetRef = GHC.error "Prim: get reference"

rwPrimBind :: GHC.Monad m => m a -> (a -> m b) -> m b
rwPrimBind = (GHC.>>=)

rwPrimReturn :: GHC.Monad m => a -> m a
rwPrimReturn = GHC.return

rwPrimPut :: GHC.Monad m => s -> StateT s m ()
rwPrimPut = GHC.put

rwPrimGet :: GHC.Monad m => StateT s m s
rwPrimGet = GHC.get

rwPrimSignal :: GHC.Monad m => o -> ReacT i o m i
rwPrimSignal = GHC.signal

rwPrimLift :: (GHC.MonadTrans t, GHC.Monad m) => m a -> t m a
rwPrimLift = GHC.lift

rwPrimExtrude :: GHC.Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m a
rwPrimExtrude (GHC.ReacT (GHC.StateT m)) s =
   GHC.ReacT GHC.$
     do (res,s') <- m s
        case res of
            GHC.Left y -> GHC.return (GHC.Left y)
            GHC.Right (o,k) -> GHC.return (GHC.Right (o, \ i -> rwPrimExtrude (k i) s'))

rwPrimUnfold :: ((R_, s) -> i -> PuRe s o) -> PuRe s o -> ReacT i o Identity A_
rwPrimUnfold _ (Done (a,_)) = GHC.return a
rwPrimUnfold f (Pause (o,b)) = do i <- GHC.signal o
                                  rwPrimUnfold f (f b i)

-- | Convert an Integer into a @'Finite' n@, throws an error if >= @n@.
rwPrimFinite :: KnownNat n => Integer -> Finite n
rwPrimFinite = F.finite

rwPrimFiniteMinBound :: KnownNat n => Finite n
rwPrimFiniteMinBound = GHC.minBound

rwPrimFiniteMaxBound :: KnownNat n => Finite n
rwPrimFiniteMaxBound = GHC.maxBound

rwPrimToFinite :: KnownNat n => Vec m Bool -> Finite n
rwPrimToFinite = F.finite . BW.toInteger'

rwPrimToFiniteMod :: forall m n. KnownNat n => Vec m Bool -> Finite n
rwPrimToFiniteMod v = F.finite (BW.toInteger' v `GHC.mod` (natVal (Proxy :: Proxy n)))

rwPrimFromFinite :: KnownNat m => Finite n -> Vec m Bool
rwPrimFromFinite = rwPrimResize . rwPrimBits . F.getFinite

-- *** Built-in Vec functions. ***

-- | Turns a List literal into a Vec with fixed length. I.e.,
-- > [x, y, z] :: Vec 3 a
rwPrimVecFromList :: KnownNat n => [a] -> Vec n a
rwPrimVecFromList v = case V.fromList v of
       GHC.Just v' -> v'
       GHC.Nothing -> GHC.error "failed fromList: list is a different length than expected"

rwPrimVecReplicate :: KnownNat n => a -> Vec n a
rwPrimVecReplicate = V.replicate

rwPrimVecReverse :: Vec n a -> Vec n a
rwPrimVecReverse = V.reverse

rwPrimVecSlice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecSlice = V.slice

-- | Slice indexed from the end of the Vec.
rwPrimVecRSlice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecRSlice i = V.reverse . V.slice i . V.reverse

rwPrimVecIndex :: Vec n a -> Finite n -> a
rwPrimVecIndex = V.index

rwPrimVecIndexProxy :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a
rwPrimVecIndexProxy = V.index'

rwPrimVecMap :: (a -> b) -> Vec n a -> Vec n b
rwPrimVecMap = V.map

-- rwPrimVecZip :: Vec n a -> Vec n b -> Vec n (a , b)
-- rwPrimVecZip = V.zip

rwPrimVecGenerate :: KnownNat n => (Finite n -> a) -> Vec n a
rwPrimVecGenerate = V.generate

-- rwPrimVecIterate :: KnownNat n => Proxy n -> (a -> a) -> a -> Vec n a
-- rwPrimVecIterate = V.iterateN'

-- | Concatenate vectors.
rwPrimVecConcat :: Vec n a -> Vec m a -> Vec (n + m) a
rwPrimVecConcat = (V.++)

-- | Interpret an Integer as a bit vector.
rwPrimBits :: Integer -> Vec 128 Bool
rwPrimBits =
      rwPrimVecFromList . BW.padTrunc' i
                        . GHC.reverse . BW.int2bits'
        where
          i = GHC.fromIntegral (natVal (Proxy :: Proxy 128))

-- | Truncates or zero-pads most significant bits.
rwPrimResize :: forall m n . KnownNat m => Vec n Bool -> Vec m Bool
rwPrimResize v = rwPrimVecFromList vs'
    where
      vs = V.toList v
      vs' = BW.resize' (GHC.fromEnum (natVal (Proxy :: Proxy m))) vs

rwPrimSignextend :: forall m n . KnownNat m => Vec n Bool -> Vec m Bool
rwPrimSignextend v = rwPrimVecFromList vs'
    where
      vs = V.toList v
      vs' = BW.signextend (GHC.fromEnum (natVal (Proxy :: Proxy m))) vs

-- | Update index i to value a
rwPrimVecUpdate :: KnownNat n => Vec n a -> Finite n -> a -> Vec n a
rwPrimVecUpdate v i a = V.update v (V.singleton (GHC.fromEnum i,a))

-- | Update multiple indices
-- rwPrimVecBulkUpdate :: KnownNat n => Vec n a -> Vec m (Finite n,a) -> Vec n a
-- rwPrimVecBulkUpdate v a = V.update v (V.map (BF.first fromEnum) a)

-- | Produce integer associated with type-level natural.
rwPrimNatVal :: KnownNat n => Proxy n -> Integer
rwPrimNatVal = natVal

-- | bitSlice a j i returns bits j (most significant) to i (least significant) from a (j >= i).
--   The Finite arguments must be known/literals (after inlining).
rwPrimBitSlice :: KnownNat m => Vec n Bool -> Finite n -> Finite n -> Vec m Bool
rwPrimBitSlice v j i = case V.toSized 
                           (VU.slice (GHC.fromIntegral (F.getFinite i))
                                     (GHC.fromIntegral (F.getFinite j))
                                     (V.fromSized v)) of
      GHC.Nothing -> GHC.error "rwPrimBitSlice: slice size mismatch" 
      GHC.Just w  -> w

-- | bitIndex a i == bitSlice a i i.
--   The Finite argument must be known/literal (after inlining).
rwPrimBitIndex :: Vec n Bool -> Finite n -> Bool
rwPrimBitIndex = V.index

-- *** Primitive bitwise operations based on Verilog operators. ***

-- | Add.
rwPrimAdd :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAdd v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v GHC.+ BW.toInteger' w

-- | Subtract.
rwPrimSub :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimSub v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v GHC.- BW.toInteger' w

-- | Multiply.
rwPrimMul :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMul v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v GHC.* BW.toInteger' w

-- | Divide.
rwPrimDiv :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimDiv v w = rwPrimResize $ rwPrimBits $ rwPrimToInteger v `GHC.div` rwPrimToInteger w


-- | Modulus.
rwPrimMod :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMod v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v `GHC.mod` BW.toInteger' w

-- | Exponentiation.
rwPrimPow :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimPow v w = rwPrimVecFromList $ BW.power' (V.toList v) (V.toList w)

-- | Logical and.
rwPrimLAnd :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLAnd v w = V.or v GHC.&& V.or w

-- | Logical or.
rwPrimLOr :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLOr v w = V.or v GHC.|| V.or w

-- | Logical not.
rwPrimLNot :: Vec n Bool -> Bool
rwPrimLNot v = GHC.not (V.or v)  -- note that 'or' acts as 'toBool'

-- | Bitwise and.
rwPrimAnd :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAnd = V.zipWith (GHC.&&)

-- | Bitwise or.
rwPrimOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimOr = V.zipWith (GHC.||)

-- | Bitwise not.
rwPrimNot :: Vec n Bool -> Vec n Bool
rwPrimNot = V.map GHC.not

-- | Bitwise exclusive or.
rwPrimXOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXOr = V.zipWith GHC.xor

-- | Bitwise exclusive nor.
rwPrimXNor :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXNor = V.zipWith (\ x y -> GHC.not (GHC.xor x y))

-- | Shift left.
rwPrimLShift :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimLShift v i = rwPrimVecFromList $ BW.shiftL' (V.toList v) (V.toList i)

-- | Shift right.
rwPrimRShift :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShift v i = rwPrimVecFromList $ BW.shiftR' (V.toList v) (V.toList i)

-- | Shift right, sign-extend.
rwPrimRShiftArith :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShiftArith v i = rwPrimVecFromList $ BW.arithShiftR' (V.toList v) (V.toList i)

-- | Equal.
rwPrimEq :: Vec n Bool -> Vec n Bool -> Bool
rwPrimEq v w = BW.toInteger' v GHC.== BW.toInteger' w

-- | Greater-than.
rwPrimGt :: Vec n Bool -> Vec n Bool -> Bool
rwPrimGt v w = BW.toInteger' v GHC.> BW.toInteger' w

-- | Greater-than or equal.
rwPrimGtEq :: Vec n Bool -> Vec n Bool -> Bool
rwPrimGtEq v w = BW.toInteger' v GHC.<= BW.toInteger' w

-- | Less-than.
rwPrimLt :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLt v w = BW.toInteger' v GHC.< BW.toInteger' w

-- | Less-than or equal.
rwPrimLtEq :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLtEq v w = BW.toInteger' v GHC.<= BW.toInteger' w

-- | Reduction and.
rwPrimRAnd :: Vec n Bool -> Bool
rwPrimRAnd = V.and

-- | Reduction nand.
rwPrimRNAnd :: Vec (1 + n) Bool -> Bool
rwPrimRNAnd = V.foldl1 (\ x y -> GHC.not (x GHC.&& y))

-- | Reduction or.
rwPrimROr :: Vec n Bool -> Bool
rwPrimROr = V.or

-- | Reduction nor.
rwPrimRNor :: Vec (1 + n) Bool -> Bool
rwPrimRNor = V.foldl1 (\ x y -> GHC.not (x GHC.|| y))

-- | Reduction xor.
rwPrimRXOr :: Vec (1 + n) Bool -> Bool
rwPrimRXOr = V.foldl1 GHC.xor

-- | Reduction xnor.
rwPrimRXNor :: Vec (1 + n) Bool -> Bool
rwPrimRXNor = V.foldl1 (\ x y -> GHC.not (GHC.xor x y))

-- | Most significant bit.
rwPrimMSBit :: Vec (1 + n) Bool -> Bool
rwPrimMSBit = V.head

-- rwPrimToInteger :: Vec n Bool -> Integer
-- rwPrimToInteger = V.foldr (\ b iacc -> if b then 1 GHC.+ 2 GHC.* iacc else 2 GHC.* iacc) 0

rwPrimToInteger :: Vec n Bool -> Integer
rwPrimToInteger = V.foldl (\ iacc b -> if b then 1 GHC.+ 2 GHC.* iacc else 2 GHC.* iacc) 0
