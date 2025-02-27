{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.HSE.Rename
      ( Renamer, fixFixity, getExports, allExports
      , exclude, extend, finger, rename
      , FQName (mod, name), qnamish
      , QNamish
      , Namespace (..)
      , Exports, expValue, expType, expFixity, expCtorSigs, getCtors
      , Ctors, FQCtors, CtorSigs, FQCtorSigs
      , setCtors, getLocalTypes, getLocalCtorSigs
      , lookupCtors, lookupCtorSig, lookupCtorSigsForType
      , findCtorSigFromField
      , toFilePath
      , fromImps
      ) where

import ReWire.Annotation (Annotation, Annote, noAnn)
import ReWire.Error (failAt, mark, MonadError, AstError)
import ReWire.HSE.Fixity (fixLocalOps, deuniquifyLocalOps)
import ReWire.Orphans ()
import ReWire.Pretty (TextShow, FromGeneric (..))

import Control.Arrow ((&&&), first)
import Control.Monad (foldM, void)
import Control.Monad.State (MonadState)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Language.Haskell.Exts.Fixity (Fixity (..), AppFixity (..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo, noSrcSpan)
import System.FilePath (joinPath, (<.>))

import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Language.Haskell.Exts.Syntax           as S

import Language.Haskell.Exts.Syntax hiding (Namespace, Annotation, Module)

-- | Map from type name to its set of data constructors.
--   Note: the set of "ctors" also includes fields (things that might appear in
--   an export list).
type Ctors = HashMap (Name ()) (HashSet (Name ()))

-- | Qualified (globally-unique) version of the above map.
type FQCtors = HashMap FQName (HashSet FQName)

-- | Map from construtor name to its field "signature,"
--   which is a list of field names and types.
type CtorSigs = HashMap (Name ()) [(Maybe (Name ()), Type ())]

-- | Qualified (globally-unique) version of the above map.
type FQCtorSigs = HashMap FQName [(Maybe FQName, Type ())]

-- Note that GHC (although we might not catch this) disallows the same symbol
-- appearing twice in an export list (e.g., with different qualifiers, from
-- different modules), but clearly you can import the same symbol (defined in
-- the same or different modules) twice with different qualifiers.
data Exports = Exports
      { expValues      :: !(HashSet FQName)                  -- Values
      , expTypes       :: !(HashSet FQName)                  -- Types
      , expFixities    :: !(HashSet Fixity)                  -- Fixities
      , expCtors       :: !FQCtors
      , expCtorSigs    :: !FQCtorSigs
      }
      deriving (Show, Generic)
      deriving TextShow via FromGeneric Exports

expValue :: FQName -> Exports -> Exports
expValue x e@Exports { expValues } = e { expValues = Set.insert x expValues }

expType :: FQName -> HashSet FQName -> FQCtorSigs -> Exports -> Exports
expType x cs' sigs' e@Exports { expValues, expTypes, expCtors, expCtorSigs } = e
      { expValues      = cs' <> expValues
      , expTypes       = Set.insert x expTypes
      , expCtors       = Map.unionWith mappend
                              (Map.fromList [(x, cs'), (qnamish $ name x, cs')]) -- Insert both qualified and unqualified keys for pre-renamer lookup.
                              expCtors
      , expCtorSigs    = sigs' <> expCtorSigs
      }

expFixity :: Assoc () -> Int -> Name () -> Exports -> Exports
expFixity asc lvl x e@Exports { expFixities } = e { expFixities = Set.insert (Fixity asc lvl $ UnQual () x) expFixities }

-- | Things in the export list of the named thing (ctors or fields).
getCtors :: QNamish a => a -> Exports -> HashSet FQName
getCtors x Exports { expCtors } = fromMaybe mempty $ Map.lookup (qnamish x) expCtors

fixities :: Exports -> [Name ()] -> [Fixity]
fixities Exports { expFixities } ns = Set.toList $ Set.filter (\ (Fixity _ _ n') -> n' `elem` map (UnQual ()) ns) expFixities

instance Semigroup Exports where
      (Exports a b c d e) <> (Exports a' b' c' d' e') =
            Exports (a <> a') (b <> b') (c <> c') (Map.unionWith mappend d d') (e <> e')

instance Monoid Exports where
      mempty = Exports mempty mempty mempty mempty mempty

data Namespace = Type | Value
      deriving (Ord, Eq, Show, Generic)
      deriving TextShow via FromGeneric Namespace

instance Hashable Namespace

data FQName = FQName { mod :: !(ModuleName ()), name :: !(Name ())}
      deriving (Ord, Eq, Show, Generic)
      deriving TextShow via FromGeneric FQName

instance Hashable FQName

-- | Sometimes-partial conversion between name-like things.
class QNamish a where
      toQNamish :: a -> QName ()
      fromQNamish :: QName l -> a

qnamish :: (QNamish a, QNamish b) => a -> b
qnamish = fromQNamish . toQNamish

instance QNamish (QName ()) where
      toQNamish = id
      fromQNamish = void
instance QNamish (QName Annote) where
      toQNamish = void
      fromQNamish = (noAnn <$)
instance QNamish (Name ()) where
      toQNamish = UnQual () . void
      fromQNamish (Qual _ _ n) = void n
      fromQNamish (UnQual _ n) = void n
      fromQNamish n            = void $ Ident () $ prettyPrint n
instance QNamish (Name SrcSpanInfo) where
      toQNamish = UnQual () . void
      fromQNamish (Qual _ _ n) = noSrcSpan <$ n
      fromQNamish (UnQual _ n) = noSrcSpan <$ n
      fromQNamish n            = (noSrcSpan <$) $ Ident noSrcSpan $ prettyPrint n
instance QNamish (Name Annote) where
      toQNamish = UnQual () . void
      fromQNamish (Qual _ _ n) = noAnn <$ n
      fromQNamish (UnQual _ n) = noAnn <$ n
      fromQNamish n            = (noAnn <$) $ Ident noAnn $ prettyPrint n
instance QNamish FQName where
      toQNamish (FQName m n) = Qual () m n
      fromQNamish (Qual _ m n) = FQName (void m) (void n)
      fromQNamish (UnQual _ n) = FQName (ModuleName () "") $ void n
      -- ^ This definition kinda defeats the purpose of FQName.
      fromQNamish n            = FQName (ModuleName () "") $ Ident () $ prettyPrint n
instance QNamish Text where
      toQNamish = UnQual () . Ident () . unpack
      fromQNamish (Qual _ (ModuleName _ "") (Ident _ x))  = pack x
      fromQNamish (Qual _ (ModuleName _ "") (Symbol _ x)) = pack x
      fromQNamish (Qual _ (ModuleName _ m) (Ident _ x))   = pack m <> "." <> pack x
      fromQNamish (Qual _ (ModuleName _ m) (Symbol _ x))  = pack m <> "." <> pack x
      fromQNamish (UnQual _ (Ident _ x))                  = pack x
      fromQNamish (UnQual _ (Symbol _ x))                 = pack x
      fromQNamish n                                       = pack $ prettyPrint n

data Renamer = Renamer
      { rnNames    :: !(HashMap (Namespace, QName ()) FQName)
      , rnExports  :: !(HashMap (ModuleName ()) Exports)
      , rnFixities :: !(HashSet Fixity)
      , rnCtors    :: !Ctors
      , rnCtorSigs :: !CtorSigs
      } deriving (Show, Generic)
      deriving TextShow via FromGeneric Renamer

instance Semigroup Renamer where
      (Renamer a b c d e) <> (Renamer a' b' c' d' e') = Renamer (a <> a') (b <> b') (c <> c') (Map.unionWith (<>) d d') (e <> e')

instance Monoid Renamer where
      mempty = Renamer mempty mempty mempty mempty mempty

rename :: (QNamish a, QNamish b) => Namespace -> Renamer -> a -> b
rename ns Renamer { rnNames } x = fromQNamish . maybe (toQNamish x) toQNamish $ Map.lookup (ns, toQNamish x) rnNames

extend :: QNamish a => Namespace -> [(a, FQName)] -> Renamer -> Renamer
extend ns kvs rn@Renamer { rnNames } = rn { rnNames = Map.fromList (map (((ns,) . toQNamish . fst) &&& snd) kvs) `Map.union` rnNames }

exclude :: QNamish a => Namespace -> [a] -> Renamer -> Renamer
exclude ns ks rn@Renamer { rnNames } = rn { rnNames = foldr (Map.delete . (ns,) . toQNamish) rnNames ks }

fixFixity :: (MonadFail m, MonadState AstError m) => Renamer -> S.Module SrcSpanInfo -> m (S.Module SrcSpanInfo)
fixFixity Renamer { rnFixities } m = do
      mark $ ann m
      deuniquifyLocalOps <$> (fixLocalOps m >>= applyFixities (Set.toList rnFixities))

addFixities :: [Fixity] -> Renamer -> Renamer
addFixities fixities' rn@Renamer { rnFixities } = rn { rnFixities = rnFixities `Set.union` Set.fromList fixities' }

addExports :: ModuleName () -> Exports -> Renamer -> Renamer
addExports m exps rn@Renamer { rnExports } = rn { rnExports = Map.insert m exps rnExports }

getExports :: ModuleName () -> Renamer -> Exports
getExports m Renamer { rnExports } = fromMaybe mempty $ Map.lookup m rnExports

allExports :: Renamer -> Exports
allExports Renamer { rnExports } = mconcat $ Map.elems rnExports

requalFixity :: ModuleName () -> Fixity -> Fixity
requalFixity m (Fixity asc lvl n) = Fixity asc lvl $ case n of
      Qual _ _ n' -> Qual () m n'
      UnQual _ n' -> Qual () m n'
      n'          -> Qual () m $ qnamish n'

filterFixities :: (Name () -> Bool) -> [Fixity] -> [Fixity]
filterFixities p = filter $ \ (Fixity _ _ n) -> p $ qnamish n

-- | True iff an entry for the name exists in the renamer.
finger :: QNamish a => Namespace -> Renamer -> a -> Bool
finger ns Renamer { rnNames } = flip Map.member rnNames . (ns,) . toQNamish

toFilePath :: ModuleName a -> FilePath
toFilePath (ModuleName _ n) = joinPath (splitOn "." n) <.> "hs"

lookupExp :: (Annotation a, MonadError AstError m) => Namespace -> Name a -> Exports -> m FQName
lookupExp ns x Exports {expValues, expTypes} = case ns of
      Value -> lkup expValues
      Type  -> lkup expTypes
      where lkup :: MonadError AstError m => HashSet FQName -> m FQName
            lkup xs = maybe (failAt (ann x) $ "Attempting to import an unexported symbol: " <> pack (prettyPrint x)) pure
                    $ find cmp (Set.toList xs)

            cmp :: FQName -> Bool
            cmp (FQName _ x') = void x == x'

getLocalTypes :: Renamer -> HashSet (Name ())
getLocalTypes rn = Map.keysSet $ rnCtors rn

getLocalCtorSigs :: Renamer -> CtorSigs
getLocalCtorSigs = rnCtorSigs

lookupCtors :: QNamish a => Renamer -> a -> HashSet FQName
lookupCtors rn x = Map.findWithDefault mempty (rename Type rn x) $ allCtors rn

lookupCtorSig :: QNamish a => Renamer -> a -> [(Maybe FQName, Type ())]
lookupCtorSig rn x = Map.findWithDefault mempty (rename Value rn x) $ allCtorSigs rn

lookupCtorSigsForType :: QNamish a => Renamer -> a -> FQCtorSigs
lookupCtorSigsForType rn x = Map.mapWithKey (const . lookupCtorSig rn) $ Set.toMap $ lookupCtors rn x

findCtorSigFromField :: QNamish a => Renamer -> a -> Maybe (FQName, [(Maybe FQName, Type ())])
findCtorSigFromField rn f = find (any ((== Just (qnamish f)) . fst) . snd) $ Map.toList (allCtorSigs rn)

noExps :: Maybe (Bool, Imports SrcSpanInfo)
noExps = Nothing

setCtors :: Ctors -> CtorSigs -> Renamer -> Renamer
setCtors ctors sigs rn = rn { rnCtors = ctors, rnCtorSigs = sigs }

qualifyCtors :: Renamer -> Ctors -> FQCtors
qualifyCtors rn = Map.mapKeys (rename Type rn) . Map.map (Set.map $ rename Value rn)

qualifyCtorSigs :: Renamer -> CtorSigs -> FQCtorSigs
qualifyCtorSigs rn = Map.mapKeys (rename Value rn) . Map.map (map $ first (fmap (rename Value rn)))

allCtors :: Renamer -> FQCtors
allCtors rn = qualifyCtors rn (rnCtors rn) <> expCtors (allExports rn)

allCtorSigs :: Renamer -> FQCtorSigs
allCtorSigs rn = qualifyCtorSigs rn (rnCtorSigs rn) <> expCtorSigs (allExports rn)

type Imports a = ([(Namespace, Name a)], [Fixity])

-- | Build renamer from a single import line. Should work on either pre- or
-- post-desugared import lists. Params: module being imported, "qualified",
-- exports (of this import), "... as <qualifier>", imports.
fromImps :: (Functor m, MonadError AstError m) => ModuleName () -> Bool -> Exports -> Maybe (ModuleName ()) -> Maybe (ImportSpecList SrcSpanInfo) -> m Renamer
fromImps m quald exps Nothing   Nothing                          = addExports m exps <$> fromImps' m quald noExps exps
fromImps m quald exps (Just m') Nothing                          = addExports m exps <$> fromImps' m' quald noExps exps
fromImps m quald exps (Just m') (Just (ImportSpecList _ h imps)) = do
      imps' <- foldM (getImp exps) mempty imps
      addExports m exps <$> fromImps' m' quald (Just (h, imps')) exps
fromImps m quald exps Nothing (Just (ImportSpecList _ h imps))   = do
      imps' <- foldM (getImp exps) mempty imps
      addExports m exps <$> fromImps' m quald (Just (h, imps')) exps

fromImps' :: (Annotation a, MonadError AstError m) => ModuleName () -> Bool -> Maybe (Bool, Imports a) -> Exports -> m Renamer
-- No list of imports -- so import everything.
fromImps' m' quald Nothing exps = fromImps' m' quald (Just (False, toImports exps)) exps
      where toImports :: Exports -> Imports SrcSpanInfo
            toImports Exports {expValues, expTypes, expFixities} =
                  ( map ((Value,) . qnamish) (Set.toList expValues) <> map ((Type,) . qnamish) (Set.toList expTypes)
                  , Set.toList $ expFixities <> Set.map (requalFixity m') expFixities
                  )
-- List of imports, no "hiding".
fromImps' m' quald (Just (False, (imps, fs))) exps = foldM ins mempty imps
      where ins :: (Annotation a, MonadError AstError m) => Renamer -> (Namespace, Name a) -> m Renamer
            ins tab (ns, x) = do
                  e <- lookupExp ns x exps
                  let xs' = (Qual () m' $ void x, e)                               : [ (UnQual () $ void x, e) | not quald ]
                      fs' = map (requalFixity m') (filterFixities (== void x) fs) <> if quald then [] else filterFixities (== void x) fs
                  pure $ extend ns xs' $ addFixities fs' tab
-- List of imports with "hiding" -- import everything, then delete the items
-- from the list.
fromImps' m' quald (Just (True, (imps, fs))) exps = flip (foldl' del) imps <$> fromImps' m' quald noExps exps
      where del :: Renamer -> (Namespace, Name a) -> Renamer
            del tab (ns, x) = exclude ns [Qual () m' $ void x, UnQual () $ void x]
                  $ addFixities (filterFixities (/= void x) fs) tab

getImp :: (MonadError AstError m) => Exports -> Imports SrcSpanInfo -> ImportSpec SrcSpanInfo -> m (Imports SrcSpanInfo)
getImp exps (imps, fs) = \ case
      IVar _ x          -> do
            _ <- lookupExp Value x exps
            pure ((Value, x) : imps, fixities exps [void x] <> fs)
      IAbs _ _ x        -> do
            _ <- lookupExp Type x exps
            pure ((Type, x) : imps, fs)
      IThingAll _ x     -> do
            _ <- lookupExp Type x exps
            pure ( (Type, x) : (map ((Value,) . qnamish) (Set.toList $ getCtors (void x) exps) <> imps)
                   , fixities exps (map name (Set.toList $ getCtors (void x) exps)) <> fs
                   )
      IThingWith _ x cs -> do
            _ <- lookupExp Type x exps
            mapM_ (flip (lookupExp Value) exps . toName) cs
            pure ( (Type, x) : (map ((Value,) . toName) cs <> imps)
                   , fixities exps (map (void . toName) cs) <> fs
                   )
      where toName :: CName a -> Name a
            toName = \ case
                  VarName _ x -> x
                  ConName _ x -> x
