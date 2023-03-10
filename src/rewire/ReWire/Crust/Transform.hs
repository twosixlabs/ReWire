{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings, MultiWayIf #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.Crust.Transform
      ( inline, expandTypeSynonyms, reduce
      , neuterExterns
      , shiftLambdas
      , liftLambdas
      , fullyApplyDefs
      , purgeUnused
      , prePurify
      , simplify
      , specialize
      , removeExpTypeAnn
      ) where

import ReWire.Unbound
      ( Fresh (..), s2n, n2s, bn2s
      , substs, subst, unembed
      , isFreeName, runFreshM
      , Name (..)
      , unsafeUnbind
      )
import ReWire.Annotation (Annote (..), Annotated (..), unAnn)
import ReWire.SYB
import ReWire.Crust.Syntax
import ReWire.Crust.TypeCheck (typeCheckDefn)
import ReWire.Error (AstError, MonadError)
import ReWire.Fix (fix, fix', boundedFix)

import Control.Arrow (first, (&&&))
import Control.Monad (zipWithM, replicateM, (>=>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (State, evalStateT, execState, StateT (..), get, gets, modify)
import Control.Monad.Writer (MonadWriter, tell, WriterT, runWriterT)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Data (Data)
import Data.Either (lefts)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hash))
import Data.List (find, foldl', sort)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Data.Set (Set, union, (\\))
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set

-- | Inlines defs marked for inlining. Must run before lambda lifting.
inline :: MonadError AstError m => FreeProgram -> m FreeProgram
inline (ts, syns, ds) = (ts, syns, ) . flip substs ds <$> subs
      where inlineDefs :: [Defn]
            inlineDefs = filter mustInline ds

            subs :: MonadError AstError m => m [(Name Exp, Exp)]
            subs = map defnSubst <$> fix "INLINE definition expansion" 100 (pure . substs (map defnSubst inlineDefs)) inlineDefs

defnSubst :: Defn -> (Name Exp, Exp)
defnSubst (Defn _ n (Embed pt) _ (Embed e)) = runFreshM $ unbind e >>= \ case
      ([], e') -> pure (n, TypeAnn (ann e') pt e')
      _        -> error "Inlining: definition not inlinable (this shouldn't happen)"

-- | Expands type synonyms.
expandTypeSynonyms :: (MonadCatch m, MonadError AstError m, Fresh m) => FreeProgram -> m FreeProgram
expandTypeSynonyms (ts, syns, ds) = (,,) <$> expandSyns ts <*> syns' <*> expandSyns ds
      where toSubst :: TypeSynonym -> (Name TyConId, Bind [Name Ty] Ty)
            toSubst (TypeSynonym _ n (Embed (Poly t))) = (n, t)

            expandSyns :: (MonadCatch m, MonadError AstError m, Fresh m, Data d) => d -> m d
            expandSyns d = subs >>= flip substs' d

            subs :: (MonadCatch m, MonadError AstError m, Fresh m) => m [(Name TyConId, Bind [Name Ty] Ty)]
            subs = map toSubst <$> syns'

            syns' :: (MonadCatch m, MonadError AstError m, Fresh m) => m [TypeSynonym]
            syns' = fix "Type synonym expansion" 100 (substs' $ map toSubst syns) syns

            substs' :: (MonadCatch m, MonadError AstError m, Fresh m, Data d) => [(Name TyConId, Bind [Name Ty] Ty)] -> d -> m d
            substs' subs' = runT (transform $ \ case
                  TyCon _ n -> case lookup n subs' of
                        Just pt -> do
                              (vs, t') <- unbind pt
                              case length vs of
                                    0 -> pure t'
                  TyApp _ a b -> case findTyCon a of
                        Just (n, args) -> case lookup n subs' of
                              Just pt -> do
                                    (vs, t') <- unbind pt
                                    let args' = args <> [b]
                                    case length vs == length args' of
                                          True -> pure $ substs (zip vs args') t'
                  )

            findTyCon :: Ty -> Maybe (Name TyConId, [Ty])
            findTyCon = \ case
                  TyCon _ n   -> pure (n, [])
                  TyApp _ a b -> do
                        (n, args) <- findTyCon a
                        pure (n, args <> [b])
                  _           -> Nothing

-- | Replaces the second argument to Extern so we don't descend into it
--   during other transformations.
neuterExterns :: MonadCatch m => FreeProgram -> m FreeProgram
neuterExterns = runT $ transform $ \ case
      App an ex e | isExtern ex -> pure $ App an ex $ TypeAnn (ann e) (poly' $ typeOf e) $ Error (ann e) (typeOf e) "extern expression placeholder"
      where isExtern :: Exp -> Bool
            isExtern = \ case
                  (flattenApp -> Extern {} : args) -> length args == 5
                  _                                -> False

-- | Removes type annotations on expressions.
removeExpTypeAnn :: MonadCatch m => FreeProgram -> m FreeProgram
removeExpTypeAnn = runT $ transform $ \ (TypeAnn _ _ e) -> pure e

-- | Shifts vars bound by top-level lambdas into defs.
-- > g = \ x1 -> \ x2 -> e
--   becomes
-- > g x1 x2 = e
shiftLambdas :: (Fresh m, MonadCatch m) => FreeProgram -> m FreeProgram
shiftLambdas (ts, syns, vs) = (ts, syns, ) <$> mapM shiftLambdas' vs
      where shiftLambdas' :: Fresh m => Defn -> m Defn
            shiftLambdas' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> mash e

            mash :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            mash e = unbind e >>= \ case
                  (vs, Lam _ _ b) -> do
                        (v, b') <- unbind b
                        mash $ bind (vs <> [v]) b'
                  _               -> pure e

-- | Inlines everything to the left of ">>=" and
-- > (m >>= f) >>= g
-- > (m >>= (\ x -> s)) >>= g
-- becomes
-- > m >>= (\ x -> f x >>= g)
-- > m >>= (\ x -> s >>= g)
-- also
-- > m >>= f
-- becomes (to trigger lambda lifting)
-- > m >>= \ x -> f x
prePurify :: (Fresh m, MonadCatch m, MonadError AstError m) => FreeProgram -> m FreeProgram
prePurify (ts, syns, ds) = (ts, syns, ) <$> mapM ppDefn ds
      where ppDefn :: (MonadError AstError m, Fresh m) => Defn -> m Defn
            ppDefn d@Defn { defnBody = Embed e } = do
                  (xs, e') <- unbind e
                  e''      <- ppExp e'
                  pure $ d { defnBody = Embed (bind xs e'') }

            ppExp :: (MonadError AstError m, Fresh m) => Exp -> m Exp
            ppExp = \ case
                  e | needsRejiggering e -> rejiggerBind e
                  -- Inline everything on the LHS.
                  (dstBind -> Just (a, e1, e2)) -> do
                        let t = TyBlank a
                        e1' <- flatten (filter (\ d -> isReT d && inlineable d) ds) e1
                        rejiggerBind $ mkBind a t e1' e2
                  App an e1 e2 -> App an <$> ppExp e1 <*> ppExp e2
                  Lam an t  e -> do
                        (x, e') <- unbind e
                        Lam an t . bind x <$> ppExp e'
                  Case an t disc e els -> do
                        (p, e') <- unbind e
                        Case an t <$> ppExp disc <*> (bind p <$> ppExp e') <*> mapM ppExp els
                  Match an t disc p e els -> Match an t <$> ppExp disc <*> pure p <*> ppExp e <*> mapM ppExp els
                  LitList an t es -> LitList an t <$> mapM ppExp es
                  TypeAnn an pt e -> TypeAnn an pt <$> ppExp e
                  e -> pure e

            needsRejiggering :: Exp -> Bool
            needsRejiggering = \ case
                  (dstBind -> Just (_, dstBind -> Just (_, _, _), _)) -> True
                  (dstBind -> Just (_, _, e2)) | not (isLambda e2)      -> True
                  _ -> False

            rejiggerBind :: (MonadError AstError m, Fresh m) => Exp -> m Exp
            rejiggerBind = \ case
                  -- Associate bind to the right (case with a lambda on the right).
                  (dstBind -> Just (a, dstBind -> Just (_, e1, Lam _ _ e2), e3)) -> do
                        let t = TyBlank a
                        (x, e2') <- unbind e2
                        ppExp $ mkBind a t e1
                              $ Lam (ann e2') t (bind x $ mkBind a t e2' e3)
                  -- Associate bind to the right.
                  (dstBind -> Just (a, dstBind -> Just (_, e1, e2), e3)) -> do
                        let t = TyBlank a
                        x <- fresh $ s2n "rabind"
                        let e' = mkBind a t (App (ann e2) e2 $ Var (ann e2) t x) e3
                        ppExp $ mkBind a t e1 $ Lam (ann e2) t (bind x e')
                  -- Lambda-abstract the right side (so it will get lambda-lifted).
                  (dstBind -> Just (a, e1, e2)) | not (isLambda e2) -> do
                        x <- fresh $ s2n "sbind"
                        let t = TyBlank a
                        ppExp $ mkBind a t e1
                              $ Lam (ann e2) t (bind x $ App (ann e2) e2 $ Var (ann e2) t x)
                  e -> pure e

            isLambda :: Exp -> Bool
            isLambda = \ case
                  Lam {} -> True
                  _      -> False

            dstBind :: Exp -> Maybe (Annote, Exp, Exp)
            dstBind = \ case
                  App a (App _ (Var _ _ (bn2s -> "rwBind")) e1) e2 -> Just (a, e1, e2)
                  _                                                -> Nothing

            mkBind :: Annote -> Ty -> Exp -> Exp -> Exp
            mkBind a t e1 e2 = App a (App a (Var a t $ s2n "rwBind") e1) e2

            flatten :: (Fresh m, MonadError AstError m) => [Defn] -> Exp -> m Exp
            flatten ds = fix "Bind LHS definition expansion" 100 (pure . substs (map defnSubst ds))

            isReT :: Defn -> Bool
            isReT Defn { defnPolyTy = Embed (Poly (unsafeUnbind -> (_, t))) } = isResMonad t

-- | So if e :: a -> b, then
-- > g = e
--   becomes
-- > g x0 = e x0
-- except with Match, we add x0 to the list of local ids.
fullyApplyDefs :: Fresh m => FreeProgram -> m FreeProgram
fullyApplyDefs (ts, syns, vs) = (ts, syns, ) <$> mapM fullyApplyDefs' vs
      where fullyApplyDefs' :: Fresh m => Defn -> m Defn
            fullyApplyDefs' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> fullyApply e

            fullyApply :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            fullyApply e = do
                  (vs, e') <- unbind e
                  case typeOf e' of
                        TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t) _ -> do
                              x <- fresh $ s2n "$x"
                              fullyApply $ bind (vs <> [x]) $ appl t (Var (ann e') t x) e'
                        _                                             -> pure e

            appl :: Ty -> Exp -> Exp -> Exp
            appl t x = \ case
                  Match an t' e1 p e Nothing    -> Match an (arrowRight t') (mkPair an e1 x) (mkPairMPat an p $ MatchPatVar an t) e Nothing
                  Match an t' e1 p e (Just els) -> Match an (arrowRight t') (mkPair an e1 x) (mkPairMPat an p $ MatchPatVar an t) e $ Just $ appl t x els
                  e                             -> App (ann e) e x

-- | Lifts lambdas and case/match into a top level fun def.
-- TODO(chathhorn): a lot of duplicated code here.
-- TODO(chathhorn): use Writer instead of State
liftLambdas :: (Fresh m, MonadCatch m) => FreeProgram -> m FreeProgram
liftLambdas p = evalStateT (runT liftLambdas' p) []
      where liftLambdas' :: (MonadCatch m, Fresh m) => Transform (StateT [Defn] m)
            liftLambdas' =  \ case
                  Lam an t b -> do
                        (x, e)    <- unbind b
                        -- The only free names should be globally-bound
                        -- variables (and x) at this point and we can also
                        -- assume every bound name in e' was bound at a level above e'.
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr arr (typeOf e') $ map snd bvs <> [t]
                        f     <- fresh $ s2n "$LL.lambda"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind (fvs <> [x]) e')
                        pure $ foldl' (App an) (Var an t' f) $ map (toVar an . first promote) bvs
                  Case an t e1 b e2 -> do
                        (p, e)    <- unbind b
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let pvs = patVars p

                        let t' = foldr arr (typeOf e) $ map snd pvs <> map snd bvs
                        f     <- fresh $ s2n "$LL.case"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind (map fst pvs <> fvs) e')
                        let lvars = map (toVar an . first promote) bvs
                        pure $ Match an t (mkTuple an $ [e1] <> lvars)
                                    (mkTupleMPat an $  [transPat p] <> map (MatchPatVar an . typeOf) lvars)
                                    (Var an t' f) e2
                  -- TODO(chathhorn): This case is really just normalizing Match, consider moving to ToCore.
                  Match an t e1 p e els | liftable e -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.match"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        let lvars = map (toVar an) bvs
                        pure $ Match an t (mkTuple an $ [e1] <> lvars)
                                    (mkTupleMPat an $ [p] <> map (MatchPatVar an . typeOf) lvars)
                                    (Var an t' f) els
                  -- Lifts matches in the operator position of an application.
                  -- TODO(chathhorn): move somewhere else?
                  App an e@Match {} arg -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.matchapp"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        pure $ App an
                              (foldl' (App an) (Var an t' f) $ map (toVar an) bvs)
                              arg
                  -- Lifts the first argument to extrude (required for purification).
                  App an ex@(Var _ _ (bn2s -> "extrude")) e | liftable e && not (isExtrude e) -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) (typeOf e) bvs
                        f     <- fresh $ s2n "$LL.extrude"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        pure $ App an ex (foldl' (App an) (Var an t' f) $ map (toVar an) bvs)
                  ||> (\ ([] :: [Defn]) -> get) -- this is cute!
                  ||> TId

            isExtrude :: Exp -> Bool
            isExtrude = \ case
                  App _ (App _ (Var _ _ (bn2s -> "extrude")) _) _ -> True
                  _                                               -> False

            liftable :: Exp -> Bool
            liftable = \ case
                  Var {} -> False
                  _      -> True

            toVar :: Annote -> (Name Exp, Ty) -> Exp
            toVar an (v, vt) = Var an vt v

            freshen :: (MonadCatch m, Fresh m) => Exp -> m ([Name Exp], Exp)
            freshen e = do
                  let bvs  = bv e
                  fvs      <- replicateM (length bvs) $ fresh $ s2n "$LL"
                  e'       <- substs' (zip (map fst bvs) fvs) e
                  pure (fvs, e')

            substs' :: MonadCatch m => [(Name Exp, Name Exp)] -> Exp -> m Exp
            substs' subs = runT (transform $ \ n -> pure $ fromMaybe n (lookup n subs))

            bv :: Data a => a -> [(Name Exp, Ty)]
            bv = nubOrdOn fst . runQ (query $ \ case
                  Var _ t n | not $ isFreeName n -> [(n, t)]
                  _                              -> [])

            patVars :: Pat -> [(Name Exp, Ty)]
            patVars = \ case
                  PatCon _ _ _ ps      -> concatMap patVars ps
                  PatVar _ (Embed t) x -> [(x, t)]
                  _                    -> []

            promote :: Name a -> Name a
            promote = \ case
                  Bn l k | l >= 0 -> Bn (l - 1) k
                  b               -> b

            transPat :: Pat -> MatchPat
            transPat = \ case
                  PatCon an (Embed t) (Embed c) ps -> MatchPatCon an t c $ map transPat ps
                  PatVar an (Embed t) _            -> MatchPatVar an t
                  PatWildCard an (Embed t)         -> MatchPatWildCard an t

-- | Remove unused definitions.
purgeUnused :: Text -> FreeProgram -> FreeProgram
purgeUnused start (ts, syns, vs) = (inuseData (fix' extendWithCtorParams $ externCtors vs') (fv $ trec vs') ts, syns, vs')
      where vs' :: [Defn]
            vs' = inuseDefn start vs

            inuseDefn :: Text -> [Defn] -> [Defn]
            inuseDefn start ds = map toDefn $ Set.elems $ execState (inuseDefn' ds') ds'
                  where inuseDefn' :: Set (Name Exp) -> State (Set (Name Exp)) ()
                        inuseDefn' ns | Set.null ns = pure () -- TODO(chathhorn): rewrite using fix?
                                      | otherwise   = do
                              inuse  <- get
                              modify $ union $ fvs ns
                              inuse' <- get
                              inuseDefn' $ inuse' \\ inuse

                        reservedDefn :: [Text]
                        reservedDefn =
                                     [ start
                                     , "unfold"
                                     ]

                        ds' :: Set (Name Exp)
                        ds' = Set.fromList $ filter (flip elem reservedDefn . n2s) $ map defnName ds

                        fvs :: Set (Name Exp) -> Set (Name Exp)
                        fvs = Set.fromList . concatMap (fv . unembed . defnBody . toDefn) . Set.elems

                        toDefn :: Name Exp -> Defn
                        toDefn n | Just d <- find ((== n) . defnName) ds = d
                                 | otherwise                             = error $ "Something went wrong: can't find symbol: " <> show n


            inuseData :: [Name TyConId] -> [Name DataConId] -> [DataDefn] -> [DataDefn]
            inuseData ts ns = filter (not . null . dataCons) . map (inuseData' ts ns)

            inuseData' :: [Name TyConId] -> [Name DataConId] -> DataDefn -> DataDefn
            inuseData' ts ns d@(DataDefn an n k cs)
                  | n     `elem` ts           = d
                  | n2s n `elem` reservedData = d
                  | otherwise                 = DataDefn an n k $ filter ((`Set.member` Set.fromList ns) . dataConName) cs

            reservedData :: [Text]
            reservedData =
                         [ "PuRe"
                         , "(,)"
                         , "()"
                         , "Bit"
                         ]

            -- | Also treat as used: all ctors for types returned by externs and ReT inputs.
            externCtors :: Data a => a -> [Name TyConId]
            externCtors = runQ $ (\ case
                        e@Extern {} -> ctorNames $ flattenAllTyApp $ rangeTy $ typeOf e
                        e@Bits {}   -> ctorNames $ flattenAllTyApp $ rangeTy $ typeOf e
                        _           -> [])
                  ||? (\ case
                        Defn _ (n2s -> n) (Embed (Poly (unsafeUnbind -> (_, t)))) _ _ | n == start -> maybe [] (ctorNames . flattenAllTyApp) $ resInputTy t
                        _                                                                          -> [])
                  ||? QEmpty

            extendWithCtorParams :: [Name TyConId] -> [Name TyConId]
            extendWithCtorParams = nubOrd . sort . foldr extend' []
                  where extend' :: Name TyConId -> [Name TyConId] -> [Name TyConId]
                        extend' n = (<> concatMap (concatMap ctorTypes . dataCons) (filter ((== n2s n) . n2s . dataName) ts))

                        ctorTypes :: DataCon -> [Name TyConId]
                        ctorTypes (DataCon _ _ (Embed (Poly (unsafeUnbind -> (_, t))))) = ctorNames $ flattenAllTyApp t

            dataConName :: DataCon -> Name DataConId
            dataConName (DataCon _ n _) = n

            ctorNames :: [Ty] -> [Name TyConId]
            ctorNames = \ case
                  TyCon _ n : cs -> n : ctorNames cs
                  _ : cs         -> ctorNames cs
                  _              -> []

-- | Repeatedly calls "reduce" and "specialize" -- attempts to remove
-- higher-order functions by partially evaluating them.
simplify :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
simplify = flip evalStateT mempty . boundedFix tst 10 (specialize >=> reduce)
      where tst :: FreeProgram -> FreeProgram -> Bool
            tst (_, _, vs) (_, _, vs') = hash (unAnn vs) == hash (unAnn vs')

type SpecState = StateT (HashMap (Name Exp, [AppSig]) Defn)
type AppSig = Maybe Exp

-- | If b has no bound variables (i.e., not bound by a global def), then
-- > f :: A -> Y
-- > f = \ a -> g a b
-- > g :: A -> X -> Y
-- > g = ...g...
--   becomes
-- > f :: A -> Y
-- > f = \ a -> g' a
-- > g :: A -> X -> Y
-- > g = ...g...
-- > g' :: A -> Y
-- > g' = \ a' -> (...g...) a' b
specialize :: (MonadError AstError m, Fresh m) => FreeProgram -> SpecState m FreeProgram
specialize (ts, syns, vs) = do
      vs'     <- mapM specDefn vs
      newDefs <- gets $ filter (not . isGlobal . defnName) . Map.elems
      pure (ts, syns, vs' <> newDefs)
      where gs :: HashMap (Name Exp) Defn
            gs = Map.fromList $ map (defnName &&& id) vs

            isGlobal :: Name Exp -> Bool
            isGlobal = flip Map.member gs

            specDefn :: (MonadError AstError m, Fresh m) => Defn -> SpecState m Defn
            specDefn (Defn ann n pt inl (Embed body)) = do
                  (vs, body') <- unbind body
                  Defn ann n pt inl <$> Embed <$> bind vs <$> specExp body'

            specExp :: (MonadError AstError m, Fresh m) => Exp -> SpecState m Exp
            specExp = \ case
                  e@(App an e' a') | Var _ _ g : args <- flattenApp e
                                   , Just d           <- Map.lookup g gs
                                   , inlineable d
                                               -> do
                        args' <- mapM specExp args
                        let s = sig args'
                        if | all isNothing s -> App an <$> specExp e' <*> specExp a'
                           | otherwise       -> do
                              defs <- get
                              d' <- case Map.lookup (g, s) defs of
                                    Just d' -> pure d'
                                    _       -> do
                                          d' <- mkDefn s d
                                          modify $ Map.insert (g, s) d'
                                          pure d'
                              mkApp an (defnName d') <$> getTy d' <*> pure args' <*> pure s
                  App an e arg                 -> App an <$> specExp e <*> specExp arg
                  Lam an t b                   -> do
                        (vs, b') <- unbind b
                        Lam an t . bind vs <$> specExp b'
                  Case an t e p (Just e')      -> do
                        (ps, p') <- unbind p
                        Case an t <$> specExp e <*> (bind ps <$> specExp p') <*> (Just <$> specExp e')
                  Case an t e p Nothing        -> do
                        (ps, p') <- unbind p
                        Case an t <$> specExp e <*> (bind ps <$> specExp p') <*> pure Nothing
                  Match an t e p e' (Just e'') -> Match an t <$> specExp e <*> pure p <*> specExp e' <*> (Just <$> specExp e'')
                  Match an t e p e' Nothing    -> Match an t <$> specExp e <*> pure p <*> specExp e' <*> pure Nothing
                  LitList an t es              -> LitList an t <$> mapM specExp es
                  TypeAnn an pt e              -> TypeAnn an pt <$> specExp e
                  e                            -> pure e

            sig :: [Exp] -> [AppSig]
            sig es = map sig' es
                  where sig' :: Exp -> AppSig
                        sig' = \ case
                              e | all isGlobal $ fv e -> Just $ unAnn e
                              _                       -> Nothing

            getTy :: Fresh m => Defn -> m Ty
            getTy Defn { defnPolyTy = Embed (Poly pt) } = snd <$> unbind pt

            mkDefn :: (MonadError AstError m, Fresh m) => [AppSig] -> Defn -> m Defn
            mkDefn s (Defn an g (Embed (Poly bgt)) inl (Embed body)) = do
                  g'              <- fresh g
                  gt              <- snd <$> unbind bgt
                  (tinfo, t')     <- mkTy s gt
                  (bodyvs, body') <- unbind body
                  typeCheckDefn ts vs
                       $ Defn an g' (Embed $ Poly $ bind (fv t') t') inl
                       $ Embed $ bind [] $ mkLam (lefts tinfo) $ foldl' (App an) (mkLam (zip (fst $ flattenArrow gt) bodyvs) (TypeAnn (ann body') (Poly bgt) body'))
                       $ map (either (uncurry $ Var an) id) tinfo

                  where mkLam :: [(Ty, Name Exp)] -> Exp -> Exp
                        mkLam vs b = foldr (\ (t, v) e -> Lam an (t `arr` typeOf e) $ bind v e) b vs

            mkTy :: Fresh m => [AppSig] -> Ty -> m ([Either (Ty, Name Exp) Exp], Ty)
            mkTy s (flattenArrow -> (gtas, gtr)) = do
                  (gtas', subs) <- runWriterT $ zipWithM mkTy' gtas s
                  pure $ substs subs (gtas', foldr arr gtr $ map fst (lefts gtas') <> drop (length gtas') gtas)
                  where mkTy' :: Fresh m => Ty -> AppSig -> WriterT [(Name Ty, Ty)] m (Either (Ty, Name Exp) Exp)
                        mkTy' t = \ case
                              Just e -> do
                                    unify t (typeOf e)
                                    pure $ Right e
                              Nothing -> do
                                    n <- fresh $ s2n "sp"
                                    pure $ Left (t, n)

                        unify :: MonadWriter [(Name Ty, Ty)] m => Ty -> Ty -> m ()
                        unify (TyApp _ tl tr) (TyApp _ tl' tr')                = unify tl tl' >> unify tr tr'
                        unify (TyVar _ _ u)   t'                               = tell [(u, t')]
                        unify _               _                                = pure ()

            mkApp :: Annote -> Name Exp -> Ty -> [Exp] -> [AppSig] -> Exp
            mkApp an g t es = foldl' (App an) (Var an t g) . catMaybes . zipWith mkApp' es
                  where mkApp' :: Exp -> AppSig -> Maybe Exp
                        mkApp' e = maybe (Just e) (const Nothing)

data MatchResult = MatchYes ![(Name Exp, Exp)]
                 | MatchMaybe
                 | MatchNo
      deriving Show

-- | Partially evaluate expressions.
reduce :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
reduce (ts, syns, vs) = (ts, syns, ) <$> mapM reduceDefn vs
      where reduceDefn :: (Fresh m, MonadError AstError m) => Defn -> m Defn
            reduceDefn (Defn an n pt b (Embed e)) = unbind e >>= \ case
                  (vs, e') -> (Defn an n pt b . Embed) . bind vs <$> reduceExp e'

            reduceExp :: (Fresh m, MonadError AstError m) => Exp -> m Exp
            reduceExp = \ case
                  App an e1 e2      -> do
                        e1' <- reduceExp e1
                        e2' <- reduceExp e2
                        case e1' of
                              Lam _ _ e -> do
                                    (x, e') <- unbind e
                                    reduceExp $ subst x e2' e'
                              TypeAnn _ _ (Lam _ _ e) -> do
                                    (x, e') <- unbind e
                                    reduceExp $ subst x e2' e'
                              _              -> pure $ App an e1' e2'
                  Lam an t e      -> do
                        (x, e') <- unbind e
                        Lam an t . bind x <$> reduceExp e'
                  Case an t e e1 e2 -> do
                        (p, e1') <- unbind e1
                        e' <- reduceExp e
                        let mr = matchPat e' p
                        case mr of
                              MatchYes sub -> reduceExp $ substs sub e1'
                              MatchMaybe   -> case e2 of
                                    Nothing  -> Case an t e' <$> (bind p <$> reduceExp e1') <*> pure Nothing
                                    Just e2' -> Case an t e' <$> (bind p <$> reduceExp e1') <*> (Just <$> reduceExp e2')
                              MatchNo      -> case e2 of
                                    Nothing  -> pure $ Error an t "Pattern match failure (reduced)"
                                    Just e2' -> reduceExp e2'
                  -- TODO(chathhorn): handle match?
                  Match an t e p e' Nothing    -> Match an t <$> reduceExp e <*> pure p <*> reduceExp e' <*> pure Nothing
                  Match an t e p e' (Just e'') -> Match an t <$> reduceExp e <*> pure p <*> reduceExp e' <*> (Just <$> reduceExp e'')
                  LitList an t es -> LitList an t <$> mapM reduceExp es
                  TypeAnn an pt (TypeAnn _ _ e) -> TypeAnn an pt <$> reduceExp e -- TODO(chathhorn)
                  TypeAnn an pt e               -> TypeAnn an pt <$> reduceExp e
                  e -> pure e

            mergeMatches :: [MatchResult] -> MatchResult
            mergeMatches []     = MatchYes []
            mergeMatches (m:ms) = case mergeMatches ms of
                  MatchYes bs -> case m of
                        MatchYes bs' -> MatchYes $ bs' <> bs
                        MatchNo      -> MatchNo
                        MatchMaybe   -> MatchMaybe
                  MatchNo     -> MatchNo
                  MatchMaybe  -> case m of
                        MatchYes _ -> MatchMaybe
                        MatchNo    -> MatchNo
                        MatchMaybe -> MatchMaybe

            matchPat :: Exp -> Pat -> MatchResult
            matchPat e = \ case
                  PatCon _ _ (Embed i) pats -> case flattenApp e of
                        Con _ _ c : es
                              | c == i && length es == length pats -> mergeMatches $ zipWith matchPat es pats
                              | otherwise                          -> MatchNo
                        _                                          -> MatchMaybe
                  PatVar _ _ x            -> MatchYes [(x, e)]
                  PatWildCard _ _         -> MatchYes []
