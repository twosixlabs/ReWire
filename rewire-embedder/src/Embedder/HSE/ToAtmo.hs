{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module Embedder.HSE.ToAtmo (toAtmo, extendWithGlobs, getImps) where

import Embedder.Annotation hiding (ann)
import Embedder.Error
import Embedder.HSE.Fixity
import Embedder.HSE.Rename
import Embedder.Atmo.Types ((|->))
import Embedder.SYB (query)

import Control.Arrow ((&&&), second)
import Control.Monad (foldM, void)
import Data.Char (isUpper)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text, pack, unpack)
import Language.Haskell.Exts.Fixity (Fixity (..))
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Set                     as Set
import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S
import qualified Embedder.Builtins              as M
import qualified Embedder.Atmo.Syntax          as M
import qualified Embedder.Atmo.Util            as M
import qualified Embedder.Atmo.Types           as M

import Language.Haskell.Exts.Syntax hiding (Annotation, Name, Kind)
import Embedder.Atmo.Util (isTupleCtor)

-- import Debug.Trace (trace)
-- import Embedder.Builtins (tb2s)


-- Name Handling:
      -- Remove s2n after renames [and in mkUid]
      -- Remove Embed
      -- remove n2s
      -- FOR NOW: remove renumber function
      -- remove binds (pretty straightforward, one for each constructor that used it)



isPrimMod :: String -> Bool
isPrimMod = (== "RWC.Primitives")

-- | An intermediate form for exports. TODO(chathhorn): get rid of it.
data Export = Export FQName
            -- ExportWith: Type name, ctors
            | ExportWith FQName (Set.Set FQName) FQCtorSigs
            | ExportAll FQName
            | ExportMod (S.ModuleName ())
            | ExportFixity (S.Assoc ()) Int (S.Name ())
      deriving Show

sDeclHead :: S.DeclHead a -> (S.Name (), [S.TyVarBind ()])
sDeclHead = \ case
      DHead _ n                          -> (void n, [])
      DHInfix _ tv n                     -> (void n, [void tv])
      DHParen _ dh                       -> sDeclHead dh
      DHApp _ (sDeclHead -> (n, tvs)) tv -> (n, tvs ++ [void tv])


{- Name Handling
mkUId :: S.Name a -> Name b
mkUId = \ case
      Ident _ n  -> s2n (pack n)
      Symbol _ n -> s2n (pack n)
-}
mkUId :: S.Name a -> Text
mkUId = \ case
      Ident _ n  -> pack n
      Symbol _ n -> pack n

-- | Translate a Haskell module into the ReWire abstract syntax.
-- Name Handling: Fresh m, rename returns an FQName which is then packaged into an Export
toAtmo :: (MonadError AstError m) => Renamer -> Module Annote -> m (M.Module, Exports)
toAtmo rn = \ case
      Module _ (Just (ModuleHead _ (ModuleName _ mname) _ exps)) _ _ (reverse -> ds) -> do
            tyDefs <- foldM (transData rn) [] ds
            tySyns <- if | isPrimMod mname -> pure [] -- TODO(chathhorn): ignore type synonyms in Embedder.hs module.
                         | otherwise       -> foldM (transTyDecl rn) [] ds
            tySigs <- foldM (transTySig rn) [] ds
            fnDefs <- foldM (transDef rn tySigs inls) [] ds
            exps'  <- maybe (pure $ getGlobExps rn ds) (\ (ExportSpecList _ exps') -> foldM (transExport rn ds) [] exps') exps
            pure (M.Module tyDefs tySyns fnDefs, resolveExports rn exps')
            where getGlobExps :: Renamer -> [Decl Annote] -> [Export]
                  getGlobExps rn ds = getTypeExports rn <> getExportFixities ds <> concatMap (getFunExports rn) ds

                  getFunExports :: Renamer -> Decl Annote -> [Export]
                  getFunExports rn = \ case
                        PatBind _ (PVar _ n) _ _ -> [Export $ rename Value rn $ void n]
                        FunBind _ ms -> map (getMatchExport rn) ms
                        _                        -> []
                  
                  getMatchExport :: Renamer -> Match Annote -> Export
                  getMatchExport rn (Match l n _ _ _) = Export $ rename Value rn $ void n
                  getMatchExport _ _ = error "getMatchExport: not a supported Match"

                  getTypeExports :: Renamer -> [Export]
                  getTypeExports rn = map (exportAll rn) $ Set.toList (getLocalTypes rn) <> tysynNames ds

                  resolveExports :: Renamer -> [Export] -> Exports
                  resolveExports rn = foldr (resolveExport rn) mempty

                  isValueName :: FQName -> Bool
                  isValueName = \ case
                        (name -> Ident _ (c : _)) | isUpper c -> False
                        _                                     -> True

                  resolveExport :: Renamer -> Export -> Exports -> Exports
                  resolveExport rn = \ case
                        Export x              | isValueName x -> expValue x
                        Export x                              -> expType x mempty mempty
                        ExportAll x                           -> expType x (lookupCtors rn x) (lookupCtorSigsForType rn x)
                        ExportWith x cs fs                    -> expType x cs fs
                        ExportMod m                           -> (<> getExports m rn)
                        ExportFixity asc lvl x                -> expFixity asc lvl x

                  inls :: Map (S.Name ()) M.DefnAttr
                  inls = foldr inl' mempty ds
                        where inl' :: Decl Annote -> Map (S.Name ()) M.DefnAttr -> Map (S.Name ()) M.DefnAttr
                              inl' = \ case
                                    InlineSig _ b Nothing (Qual _ _ x) -> Map.insert (void x) $ if b then M.Inline else M.NoInline
                                    InlineSig _ b Nothing (UnQual _ x) -> Map.insert (void x) $ if b then M.Inline else M.NoInline
                                    _                                  -> id
      m                                                                -> failAt (ann m) "Unsupported module syntax"

-- Name Handling: rename returns an FQName, which is packaged up into an Export
exportAll :: QNamish a => Renamer -> a -> Export
exportAll rn x = let x' = rename Type rn x in
      ExportWith x' (lookupCtors rn x') (lookupCtorSigsForType rn x')

getExportFixities :: [Decl Annote] -> [Export]
getExportFixities = map toExportFixity . getFixities
      where toExportFixity :: Fixity -> Export
            toExportFixity (Fixity asc lvl (S.UnQual () n)) = ExportFixity asc lvl n
            toExportFixity _                                = error "undefined"

-- Name Handling: rename to FQName to Export
transExport :: MonadError AstError m => Renamer -> [Decl Annote] -> [Export] -> ExportSpec Annote -> m [Export]
transExport rn ds exps = \ case
      EVar l (void -> x)                 ->
            if finger Value rn x
            then pure $ Export (rename Value rn x) : fixities (qnamish x) ++ exps
            else failAt l "Unknown variable name in export list"
      -- TODO(chathhorn): Ignore type exports.
      -- This is to ease compatibility with GHC -- we can have built-in type
      -- operators while ignoring parts exported from GHC.TypeLits in the
      -- RWC.Primitives module.
      EAbs _ (TypeNamespace _) _         -> pure exps
      EAbs l _ (void -> x)               ->
            if finger Type rn x
            then pure $ Export (rename Type rn x) : exps
            else failAt l "Unknown class or type name in export list"
      EThingWith l (NoWildcard _) (void -> x) cs       -> let cs' = Set.fromList $ map (rename Value rn . unwrap) cs in
            if and $ finger Type rn x : map (finger Value rn . name) (Set.toList cs')
            then pure $ ExportWith (rename Type rn x) cs' (Map.fromSet (lookupCtorSig rn) cs')
                  : concatMap (fixities . unwrap) cs ++ exps
            else failAt l "Unknown class or type name in export list"
      -- TODO(chathhorn): I don't know what it means for a wildcard to appear in the middle of an export list.
      EThingWith l (EWildcard _ _) (void -> x) _       ->
            if finger Type rn x
            then pure $ exportAll rn x : concatMap (fixities . name) (Set.toList $ lookupCtors rn x) ++ exps
            else failAt l "Unknown class or type name in export list"
      EModuleContents _ (void -> m) ->
            pure $ ExportMod m : exps
      where unwrap :: CName Annote -> S.Name ()
            unwrap (VarName _ x) = void x
            unwrap (ConName _ x) = void x

            fixities :: S.Name () -> [Export]
            fixities n = flip filter (getExportFixities ds) $ \ case
                  ExportFixity _ _ n' -> n == n'
                  _                   -> False

-- Name Handling: Fresh m, rename to Text
transData :: (MonadError AstError m) => Renamer -> [M.DataDefn] -> Decl Annote -> m [M.DataDefn]
transData rn datas = \ case
      DataDecl l _ _ (sDeclHead -> hd) cs _ -> do
            let n    = rename Type rn $ fst hd
                tvs' = map transTyVar $ snd hd

            cs'  <- mapM (transCon rn tvs' n) cs
            pure $ M.DataDefn l n tvs' cs' : datas
      _                                       -> pure datas

tysynNames :: [Decl Annote] -> [S.Name ()]
tysynNames = concatMap tySynName
      where tySynName :: Decl Annote -> [S.Name ()]
            tySynName = \ case
                  TypeDecl _ (sDeclHead -> hd) _ -> [fst hd]
                  _                              -> []

-- Name Handling: Fresh m, rename returns Text
transTyDecl :: (MonadError AstError m) => Renamer -> [M.TypeSynonym] -> Decl Annote -> m [M.TypeSynonym]
transTyDecl rn syns = \ case
      TypeDecl l (sDeclHead -> hd) t -> do
            let n   = rename Type rn $ fst hd
                lhs = map transTyVar $ snd hd
            t'  <- transTy rn t
            -- let rhs :: [Name M.Ty]
            --     rhs = fv t'

            -- let tvs' = map (renumber rhs) lhs
            pure $ M.TypeSynonym l n (lhs |-> t') : syns
      _                              -> pure syns

      -- where renumber :: [Name M.Ty] -> Name M.Ty -> Name M.Ty
      --       renumber rhs n = fromMaybe n $ find ((== n2s n) . n2s) rhs
-- FOR NOW: Let's operate on the assumption that renumber does nothing...
      -- It seems like all the numbering for all variables on either side of this should be zero.
      -- lhs used to be tvs'
-- Name Handling: renumber selects 'n : Name Ty' from 'rhs : [Name Ty]' by
--   taking one one with a matching n2s (I think that's just the fv name part)
--   and if there isn't one, then use 'n' instead
-- this is used to fix an error in transTy, it seems...
--     lhs = transTyVar (fvs from TypeDecl header)
--     rhs = fv of (transTy of (t from TypeDecl))
--     the tvs' that we keep for our TypeSynonym are lhs, renumbered using rhs 


-- Name Handling: Fresh m
transTySig :: (MonadError AstError m) => Renamer -> [(S.Name (), M.Ty)] -> Decl Annote -> m [(S.Name (), M.Ty)]
transTySig rn sigs = \ case
      TypeSig _ names t -> do
            t' <- transTy rn t
            pure $ zip (map void names) (repeat t') <> sigs
      _                 -> pure sigs

-- TODO(mheim): need the simple allUnguarded Match case, and also the non-var PatBind case
-- TODO(chathhorn): should be a map, not a fold
-- Name Handling: Fresh m, rename returns Text
transDef :: (MonadError AstError m) => Renamer -> [(S.Name (), M.Ty)] -> Map (S.Name ()) M.DefnAttr -> [M.Defn] -> Decl Annote -> m [M.Defn]
transDef rn tys inls defs = \ case
      PatBind l (PVar _ (void -> x)) (UnGuardedRhs _ e) Nothing -> do
            let x' = rename Value rn x
                t  = fromMaybe (M.TyVar l "a") $ lookup x tys
            -- Elide definition of primitives. Allows providing alternate defs for GHC compat.
            e' <- if | M.isPrim x' -> pure $ M.mkError (ann e) (Just t) $ "Prim: " <> x'
                     | otherwise   -> transExp rn e
            pure $ M.Defn l x' (M.poly' t) (Map.lookup x inls) [M.FunBinding l [] e'] : defs
      FunBind l ms@(Match _l' (void -> name) _ps _rhs _binds : _) -> do
            let name' = rename Value rn name
                t  = fromMaybe (M.TyVar l "a") $ lookup name tys
            ms' <- mapM (transFunMatch rn) ms
            pure $ M.Defn l name' (M.poly' t) (Map.lookup name inls) ms' : defs
      DataDecl       {}                                         -> pure defs -- TODO(chathhorn): elide
      InlineSig      {}                                         -> pure defs -- TODO(chathhorn): elide
      TypeSig        {}                                         -> pure defs -- TODO(chathhorn): elide
      InfixDecl      {}                                         -> pure defs -- TODO(chathhorn): elide
      TypeDecl       {}                                         -> pure defs -- TODO(chathhorn): elide
      AnnPragma      {}                                         -> pure defs -- TODO(chathhorn): elide
      MinimalPragma  {}                                         -> pure defs -- TODO(chathhorn): elide
      CompletePragma {}                                         -> pure defs -- TODO(chathhorn): elide
      RulePragmaDecl {}                                         -> pure defs -- TODO(chathhorn): elide
      DeprPragmaDecl {}                                         -> pure defs -- TODO(chathhorn): elide
      WarnPragmaDecl {}                                         -> pure defs -- TODO(chathhorn): elide
      d                                                         -> failAt (ann d) $ "Unsupported definition syntax: " <> pack (show $ void d)


-- We no longer desugar simple function bindings during HSE.Desugar
-- That is, we assume that we can have function bindings with zero guards and zero local bindings
transFunMatch :: (MonadError AstError m) => Renamer -> Match Annote -> m M.FunBinding
transFunMatch rn (Match _l name ps (UnGuardedRhs l' e) Nothing) = do
      let name' = rename Value rn name
      e' <- if | M.isPrim name' -> pure $ M.mkError (ann e) Nothing $ "Prim: " <> name'
               | otherwise   -> transExp rn e
      ps' <- mapM (transPat rn) ps
      pure $ M.FunBinding l' ps' e'
transFunMatch _ m = failAt (ann m) "transFunMatch: unsupported Match type"

-- Name Handling: used to return Name a b/c of mkUId
transTyVar :: S.TyVarBind () -> Text
transTyVar = \ case
      S.UnkindedVar _ x -> mkUId x
      S.KindedVar _ x _ -> mkUId x

-- Name Handling: Fresh m, rename returns Text
   -- previously: Renamer -> [Name M.Ty] -> Name M.TyConId ->
transCon :: (MonadError AstError m) => Renamer -> [Text] -> Text -> QualConDecl Annote -> m M.DataCon
transCon rn tvs tc = \ case
      QualConDecl l Nothing _ (ConDecl _ x tys) -> do
            let tvs' = map (M.TyVar l) tvs
            tys' <- mapM (transTy rn) tys
            let t = M.sig tys' (M.TyApp l (M.TyCon l tc) tvs')
            -- foldr M.arr (foldl' (M.TyApp l) (M.TyCon l tc) tvs') <$> mapM (transTy rn) tys
            pure $ M.DataCon l (rename Value rn x) (tvs |-> t)
      d                                         -> failAt (ann d) "Unsupported Ctor syntax"

{- Kind Handling:
takes ks : [M.Kind] after Renamer
uses them in creating TyVars (let tvs' = zipWith (M.TyVar l) ks tvs)

-}

--Var l x | Just b <- builtin x
--                             -> pure $ M.Builtin l Nothing Nothing b

flattenTyApp :: Type Annote -> [Type Annote]
flattenTyApp = \ case
      TyApp _l a b -> flattenTyApp a <> [b]
      t           -> [t]

-- Name Handling: Fresh m, rename returns Text
transTy :: (MonadError AstError m) => Renamer -> Type Annote -> m M.Ty
transTy rn = \ case
      TyForall _ _ _ t -> transTy rn t
      t@(TyApp l _ _)  -> case flattenTyApp t of
            [] -> error "Impossible flattenTyApp return"
            [_t] -> error "transTy: impossible given value of t"
            (t1 : ts) -> M.TyApp l <$> transTy rn t1 <*> mapM (transTy rn) ts
      TyCon l x | Just b <- tybuiltin (rename Type rn x)
                       -> do
                        -- _ <- trace ("ToAtmo.transTy: TyCon: tybuiltin found: " <> unpack (tb2s b)) $ pure ()
                        pure $ M.TyBuiltin l b
      TyCon l x -> pure $ M.TyCon l (rename Type rn x)
      TyVar l x        -> (pure (M.TyVar l (mkUId $ void x)))
      TyList l a       -> M.listTy l <$> transTy rn a
      TyTuple l _ ts     -> do
                        ts' <- mapM (transTy rn) ts
                        pure $ M.TyTuple l ts'
      TyPromoted l (PromotedInteger _ n _)
            | n >= 0   -> pure $ M.TyNat l $ fromInteger n
      TyInfix l a x b -> M.TyApp l (M.TyCon l (rename Type rn x')) <$> sequence [transTy rn a , transTy rn b]
            where x' | PromotedName   _ qn <- x = qn
                     | UnpromotedName _ qn <- x = qn
      t                -> failAt (ann t) $ "Unsupported type syntax: " <> pack (show t)
      where 
            tybuiltin :: S.Name Annote -> Maybe M.TyBuiltin
            tybuiltin = M.tybuiltin . pack . prettyPrint . name . rename Value rn

-- Name Handling: Fresh m, rename returns Text, except builtin function where 'name' is called on the return value
transExp :: (MonadError AstError m) => Renamer -> Exp Annote -> m M.Exp
transExp rn = \ case
      App l e1 e2            -> do
            let es = flattenApp (App l e1 e2)
            es' <- mapM (transExp rn) es
            case es' of 
                  (e' : es'') -> pure $ M.mkApp l e' es''
                  _ -> error "transExp: Impossible branch!"
      -- M.mkApp l <$> transExp rn e1 <*> (pure <$> transExp rn e2)
      Lambda l [PVar a x] e  -> do
            (vs',e') <- flattenLam rn (Lambda l [PVar a x] e)
            pure $ M.Lam l Nothing Nothing vs' e'  -- vs' =? map (mkUId . void) vs
      Var l x | Just b <- rwUserDef x
                             -> pure $ M.RWUser l Nothing Nothing b
      Var l x | Just b <- builtins x
                             -> pure $ M.RWUser l Nothing Nothing (M.RWBuiltin b)
      Var l x                -> pure $ M.Var l Nothing Nothing $ rename Value rn x
      Con l x                -> pure $ M.Con l Nothing Nothing $ rename Value rn x
      Case l e alts -> do
            e'  <- transExp rn e
            alts' <- mapM transAlt alts
            pure $ M.Case l Nothing Nothing e' alts'
      Tuple l _ es           -> do
            es' <- mapM (transExp rn) es
            pure $ M.Tuple l Nothing Nothing es'
      Lit l (Int _ n _)      -> pure $ M.LitInt l Nothing n
      Lit l (String _ s _)   -> pure $ M.LitStr l Nothing $ pack s
      List l es              -> M.LitList l Nothing Nothing <$> mapM (transExp rn) es
      ExpTypeSig _ e t       -> M.setTyAnn <$> (Just . M.poly' <$> transTy rn t) <*> transExp rn e
      e                      -> failAt (ann e) $ "Unsupported expression syntax: " <> pack (show $ void e)
      where getVars :: Pat Annote -> [S.Name ()]
            getVars p = [void x | PVar (_::Annote) x <- query p]

            transAlt :: (MonadError AstError m) => Alt Annote -> m M.PatBind
            transAlt (Alt _ p (UnGuardedRhs _ e1) _) = do
                  p'  <- transPat rn p
                  e1' <- transExp (exclude Value (getVars p) rn) e1
                  pure $ M.PatBind p' e1'
            transAlt a = failAt (ann a) $ "Unsupported Alt syntax: " <> pack (show $ void a)

            rwUserDef :: QName Annote -> Maybe M.RWUserOp
            rwUserDef = M.qn2rwu . rename Value rn
                  -- M.s2rwu . pack . prettyPrint . name . rename Value rn

            builtins :: QName Annote -> Maybe M.Builtin
            builtins =  M.builtin . pack . prettyPrint . name . rename Value rn
            
            flattenApp :: Exp Annote -> [Exp Annote]
            flattenApp = \ case
                  App _l e e' -> flattenApp e <> [e']
                  e          -> [e]
            flattenLam :: (MonadError AstError m) => Renamer -> Exp Annote 
                       -> m ([Text],M.Exp)
            flattenLam rn = \ case
                  Lambda _ [PVar _ x] e  -> do
                        (vs',e') <- flattenLam rn e -- rn =? exclude Value [void x] rn
                        let x' = rename Value rn x
                        return (x' : vs', e')
                  e -> do
                        e' <- transExp rn e
                        return ([],e')


-- Name Handling: Fresh m, rename returns Text
transPat :: (MonadError AstError m) => Renamer -> Pat Annote -> m M.Pat
transPat rn = \ case
      PApp l x ps | isTupleCtor (rename Value rn x) -> M.PatTuple l Nothing Nothing <$> mapM (transPat rn) ps
      PApp l x ps      -> M.PatCon l Nothing Nothing (rename Value rn x) <$> mapM (transPat rn) ps
      PVar l x         -> pure $ M.PatVar l Nothing Nothing (mkUId $ void x)
      PWildCard l      -> pure $ M.PatWildCard l Nothing Nothing
      PatTypeSig _ p t -> M.setTyAnn <$> (Just . M.poly' <$> transTy rn t) <*> transPat rn p
      PTuple l _b ps   -> M.PatTuple l Nothing Nothing <$> mapM (transPat rn) ps
      PAsPat l n p     -> M.PatAs l Nothing Nothing (mkUId $ void n) <$> transPat rn p
      p                -> failAt (ann p) $ "Unsupported syntax in a pattern: " <> pack (show $ void p)




-- Note: runs before desugaring.
-- TODO(chathhorn): GADT style decls?
extendWithGlobs :: Annotation a => S.Module a -> Renamer -> Renamer
extendWithGlobs = \ case
      Module l (Just (ModuleHead _ (ModuleName _ m) _ _)) _ _ ds | isPrimMod m -> extendWith ds $ ModuleName l ""
      Module _ (Just (ModuleHead _ m _ _)) _ _ ds                              -> extendWith ds m
      _                                                                        -> id
      where extendWith :: Annotation a => [Decl a] -> ModuleName a -> Renamer -> Renamer
            extendWith ds m = setCtors (getModCtors ds) (getModCtorSigs ds) . extendWith' ds (void m)

            extendWith' :: Annotation a => [Decl a] -> ModuleName () -> Renamer -> Renamer
            extendWith' ds m = extend Value (zip (getGlobValDefs ds) $ map (qnamish . S.Qual () m) $ getGlobValDefs ds)
                             . extend Type  (zip (getGlobTyDefs ds)  $ map (qnamish . S.Qual () m) $ getGlobTyDefs ds)

            getGlobValDefs :: Annotation a => [Decl a] -> [S.Name ()]
            getGlobValDefs = concatMap $ \ case
                  DataDecl _ _ _ _ cons _               -> Set.toList $ Set.unions $ map getCtor cons
                  PatBind _ (PVar _ n) _ _              -> [void n]
                  FunBind _ (Match _ n _ _ _ : _)       -> [void n]
                  FunBind _ (InfixMatch _ _ n _ _ _: _) -> [void n]
                  _                                     -> []

            getGlobTyDefs :: Annotation a => [Decl a] -> [S.Name ()]
            getGlobTyDefs = concatMap $ \ case
                  DataDecl _ _ _ hd _ _ -> [fst $ sDeclHead hd]
                  TypeDecl _ hd _       -> [fst $ sDeclHead hd]
                  _                     -> []

            getModCtors :: Annotation a => [S.Decl a] -> Ctors
            getModCtors ds =  Map.unions $ map getCtors' ds

            getCtors' :: Decl l -> Ctors
            getCtors' = \ case
                  DataDecl _ _ _ dh cons _ -> Map.singleton (fst $ sDeclHead dh) $ Set.unions $ map getCtor cons
                  _                        -> mempty

            getModCtorSigs :: Annotation a => [S.Decl a] -> CtorSigs
            getModCtorSigs ds = Map.unions $ map getCtorSigs' ds

            getCtorSigs' :: Decl l -> CtorSigs
            getCtorSigs' = \ case
                  DataDecl _ _ _ dh cons _ -> getCtorSigs (sDeclHead dh) cons
                  _                        -> mempty

            getCtorSigs :: (S.Name (), [S.TyVarBind ()]) -> [QualConDecl l] -> CtorSigs
            getCtorSigs dsig = Map.fromList . map (getCtorName &&& map (extendCtorSig $ dsigToTyApp dsig) . getFields)

            dsigToTyApp  :: (S.Name (), [S.TyVarBind ()]) -> S.Type ()
            dsigToTyApp (n, vs) = foldl' (S.TyApp ()) (S.TyCon () $ qnamish n) $ map toTyVar vs

            toTyVar :: S.TyVarBind () -> S.Type ()
            toTyVar = \ case
                  KindedVar   _ n _ -> TyVar () n
                  UnkindedVar _ n   -> TyVar () n

            extendCtorSig :: S.Type () -> (n, S.Type ()) -> (n, S.Type ())
            extendCtorSig t = second $ S.TyFun () t

            getCtor :: QualConDecl l -> Set.Set (S.Name ())
            getCtor d = Set.fromList $ getCtorName d : getFieldNames (getFields d)

            getCtorName :: QualConDecl l -> S.Name ()
            getCtorName = \ case
                  QualConDecl _ _ _ (ConDecl _ n _)        -> void n
                  QualConDecl _ _ _ (InfixConDecl _ _ n _) -> void n
                  QualConDecl _ _ _ (RecDecl _ n _)        -> void n

            getFields :: QualConDecl l -> [(Maybe (S.Name ()), S.Type ())]
            getFields = \ case
                  QualConDecl _ _ _ (ConDecl _ _ ts)         -> map ((Nothing, ) . void) ts
                  QualConDecl _ _ _ (InfixConDecl _ t1 _ t2) -> [(Nothing, void t1), (Nothing, void t2)]
                  QualConDecl _ _ _ (RecDecl _ _ fs)         -> concatMap getFields' fs

            getFields' :: FieldDecl l -> [(Maybe (S.Name ()), S.Type ())]
            getFields' (FieldDecl _ ns t) = map ((, void t) . Just . void) ns

            getFieldNames :: [(Maybe (S.Name ()), S.Type ())] -> [S.Name ()]
            getFieldNames = mapMaybe fst

getImps :: Annotation a => S.Module a -> [ImportDecl a]
getImps = \ case
      -- TODO(chathhorn): hacky. The prim/ReWire module really shouldn't depend on Prelude.
      S.Module _ (Just (ModuleHead _ (ModuleName _ m) _ _)) _ _ _ | isPrimMod m     -> [] -- all imports ignored in the Prim module.
      S.Module l (Just (ModuleHead _ (ModuleName _ "ReWire.Prelude") _ _)) _ imps _ -> addMod l "ReWire" $ filter (not . isMod "Prelude") imps -- TODO(chathhorn) gah!
      S.Module l _ _ imps _                                                         -> addMod l "ReWire.Prelude" $ addMod l "ReWire" $ filter (not . isMod "Prelude") imps
      S.XmlPage {}                                                                  -> []
      S.XmlHybrid l _ _ imps _ _ _ _ _                                              -> addMod l "ReWire.Prelude" $ addMod l "ReWire" $ filter (not . isMod "Prelude") imps
      where addMod :: Annotation a => a -> Text -> [ImportDecl a] -> [ImportDecl a]
            addMod l m imps = if any (isMod m) imps then imps
                  else ImportDecl l (ModuleName l (unpack m)) False False False Nothing Nothing Nothing : imps

            isMod :: Annotation a => Text -> ImportDecl a -> Bool
            isMod m ImportDecl { importModule = ModuleName _ n } = pack n == m
