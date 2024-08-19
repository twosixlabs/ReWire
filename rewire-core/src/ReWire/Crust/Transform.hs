{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Safe #-}
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
      , freeTyVarsToNil
      , removeMain
      ) where

import ReWire.Annotation (Annote (..), Annotated (..), unAnn)
import ReWire.Config (Config, depth)
import ReWire.Crust.Syntax (Exp (..), Ty (..), Poly (..), Pat (..), MatchPat (..), Defn (..), FreeProgram, DataCon (..), DataConId, TyConId, DataDefn (..), Builtin (..), TypeSynonym (..), flattenApp)
import ReWire.Crust.TypeCheck (typeCheckDefn, unify, TySub)
import ReWire.Crust.Types (typeOf, setTyAnn, poly, poly', flattenArrow, arr, nilTy, ctorNames, resInputTy, rangeTy, (|->), arrowRight, arrowLeft, isReacT)
import ReWire.Crust.Util (mkApp, mkError, mkLam, inlineable, mkTupleMPat, mkTuple, mkPairMPat, mkPair, mustInline)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fix, fix', boundedFix)
import ReWire.SYB (transform, transformM, query)
import ReWire.Unbound (fv, Fresh (fresh), s2n, n2s, substs, subst, unembed, isFreeName, runFreshM, Name (..), unsafeUnbind, bind, unbind, Subst (..), Alpha, Embed (Embed), Bind, trec)

import Control.Lens ((^.))
import Control.Arrow (first, (&&&))
import Control.Monad (liftM2, foldM_, zipWithM, replicateM, (>=>))
import Control.Monad.State (MonadState, evalStateT, execState, StateT (..), get, gets, modify)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Data (Data)
import Data.Either (lefts)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hash))
import Data.List (find, sort)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Data.Set (Set, union, (\\))
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set

-- import ReWire.Pretty
-- import Debug.Trace (trace)

-- | Removes the Main.main function definition, which is unused by rwc.
removeMain :: FreeProgram -> FreeProgram
removeMain (ts, syns, ds) = (ts, syns, filter (not . isMain) ds)
      where isMain :: Defn -> Bool
            isMain = (== "Main.main") . n2s . defnName

-- | Inlines defs marked for inlining. Must run before lambda lifting.
inline :: MonadError AstError m => FreeProgram -> m FreeProgram
inline (ts, syns, ds) = (ts, syns, ) . flip substs ds <$> subs
      where inlineDefs :: [Defn]
            inlineDefs = filter mustInline ds

            subs :: MonadError AstError m => m [(Name Exp, Exp)]
            subs = map defnSubst <$> fix "INLINE definition expansion" 100 (pure . substs (map defnSubst inlineDefs)) inlineDefs

defnSubst :: Defn -> (Name Exp, Exp)
defnSubst (Defn _ n (Embed pt) _ (Embed e)) = runFreshM $ unbind e >>= \ case
      ([], e') -> pure (n, setTyAnn (Just pt) e')
      _        -> error "Inlining: definition not inlinable (rwc bug)"

-- | Expands type synonyms.
expandTypeSynonyms :: (MonadError AstError m, Fresh m) => FreeProgram -> m FreeProgram
expandTypeSynonyms (ts, syns0, ds) = (,,) <$> expandSyns ts <*> syns' <*> expandSyns ds
      where toSubst :: TypeSynonym -> (Name TyConId, Bind [Name Ty] Ty)
            toSubst (TypeSynonym _ n (Embed (Poly t))) = (n, t)

            expandSyns :: (MonadError AstError m, Fresh m, Data d) => d -> m d
            expandSyns d = subs' >>= flip substs' d

            subs' :: (MonadError AstError m, Fresh m) => m [(Name TyConId, Bind [Name Ty] Ty)]
            subs' = map toSubst <$> syns'

            -- | First expand type synonyms in type synonym definitions.
            syns' :: (MonadError AstError m, Fresh m) => m [TypeSynonym]
            syns' = do
                  foldM_ checkDupe [] syns0
                  fix "Type synonym expansion" 100 (substs' $ map toSubst syns0) syns0
                  where checkDupe :: MonadError AstError m => [Text] -> TypeSynonym -> m [Text]
                        checkDupe ss (TypeSynonym an n _)
                              | n2s n `elem` ss = failAt an $ "Duplicate type synonym: " <> n2s n
                              | otherwise       = pure $ n2s n : ss

            substs' :: (MonadError AstError m, Fresh m, Data d) => [(Name TyConId, Bind [Name Ty] Ty)] -> d -> m d
            substs' subs' = transformM $ \ case
                  t@(TyCon _ n)   | Just pt <- lookup n subs'        -> do
                        (vs, t') <- unbind pt
                        pure $ if length vs == 0 then t' else t
                  t@(TyApp _ a b) | Just (n, args) <- findTyCon a
                                  , Just pt        <- lookup n subs' -> do
                        (vs, t') <- unbind pt
                        let args' = args <> [b]
                        pure $ if length vs == length args' then substs (zip vs args') t' else t
                  t                                                  -> pure t

            findTyCon :: Ty -> Maybe (Name TyConId, [Ty])
            findTyCon = \ case
                  TyCon _ n   -> pure (n, [])
                  TyApp _ a b -> do
                        (n, args) <- findTyCon a
                        pure (n, args <> [b])
                  _           -> Nothing

-- | Replaces the second argument to Extern so we don't descend into it
--   during other transformations.
neuterExterns :: FreeProgram -> FreeProgram
neuterExterns = transform $ \ case
      App an tan t ex e | isExtern ex -> App an tan t ex
                                              $ setTyAnn (poly' <$> typeOf e)
                                              $ mkError (ann e) (typeOf e) "Extern expression placeholder"
      e                               -> e
      where isExtern :: Exp -> Bool
            isExtern e = case flattenApp e of
                  [Builtin _ _ _ Extern, _ , _, _, _, _] -> True
                  _                                      -> False

-- | Shifts vars bound by top-level lambdas into defs.
-- > g = \ x1 -> \ x2 -> e
--   becomes
-- > g x1 x2 = e
shiftLambdas :: Fresh m => FreeProgram -> m FreeProgram
shiftLambdas (ts, syns, vs) = (ts, syns, ) <$> mapM shiftLambdas' vs
      where shiftLambdas' :: Fresh m => Defn -> m Defn
            shiftLambdas' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> mash e

            mash :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            mash e = unbind e >>= \ case
                  (vs, Lam _ _ _ b) -> do
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
prePurify :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
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
                        e1' <- flatten (filter (\ d -> isReacDefn d && inlineable d) ds) e1
                        e1'' <- ppExp e1'
                        e2' <- ppExp e2
                        rejiggerBind $ mkBind a e1'' e2'
                  App an tan t e1 e2 -> App an tan t <$> ppExp e1 <*> ppExp e2
                  Lam an tan t  e -> do
                        (x, e') <- unbind e
                        Lam an tan t . bind x <$> ppExp e'
                  Case an tan t disc e els -> do
                        (p, e') <- unbind e
                        Case an tan t <$> ppExp disc <*> (bind p <$> ppExp e') <*> mapM ppExp els
                  Match an tan t disc p e els -> Match an tan t <$> ppExp disc <*> pure p <*> ppExp e <*> mapM ppExp els
                  LitList an tan t es         -> LitList an tan t <$> mapM ppExp es
                  e                           -> pure e

            needsRejiggering :: Exp -> Bool
            needsRejiggering = \ case
                  (dstBind -> Just (_, dstBind -> Just (_, _, _), _)) -> True
                  (dstBind -> Just (_, _, e2)) | not (isLambda e2)    -> True
                  _                                                   -> False

            rejiggerBind :: (MonadError AstError m, Fresh m) => Exp -> m Exp
            rejiggerBind = \ case
                  -- Associate bind to the right (case with a lambda on the right).
                  -- ma :: m a
                  -- mb :: m b
                  -- fb :: b -> m c
                  -- (>>=1) :: m a -> (a -> m b) -> m b
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- (ma >>=1 \ a -> mb) >>=2 fb
                  -- becomes
                  -- (>>=1) :: m a -> (a -> m c) -> m c
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- ma >>=1 (\ a -> (mb >>=2 fb))
                  (dstBind -> Just (a, dstBind -> Just (_, e1, Lam _ _ tx e2), e3)) -> do
                        -- TODO(chathhorn): note eliding type annotation.
                        (x, e2') <- unbind e2
                        let e' = mkBind a e2' e3
                        ppExp $ mkBind a e1
                              $ Lam (ann e2') Nothing tx (bind x e')

                  -- Associate bind to the right.
                  -- fa :: a -> m b
                  -- fb :: b -> m c
                  -- (>>=1) :: m a -> (a -> m b) -> m b
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- (m >>=1 fa) >>=2 fb
                  -- becomes
                  -- (>>=1) :: m a -> (a -> m c) -> m c
                  -- (>>=2) :: m b -> (b -> m c) -> m c
                  -- m >>=1 (\ x -> fa x >>=2 fb)
                  (dstBind -> Just (a, dstBind -> Just (_, e1, e2), e3)) -> do
                        let tx = arrowLeft <$> typeOf e2
                        x <- fresh $ s2n "rabind"
                        let e' = mkBind a (mkApp (ann e2) e2 [Var (ann e2) Nothing tx x]) e3
                        ppExp $ mkBind a e1
                              $ Lam (ann e2) Nothing tx (bind x e')
                  -- Lambda-abstract the right side (so it will get lambda-lifted).
                  (dstBind -> Just (a, e1, e2)) | not (isLambda e2) -> do
                        let tx = arrowLeft <$> typeOf e2
                        x <- fresh $ s2n "sbind"
                        ppExp $ mkBind a e1
                              $ Lam (ann e2) Nothing tx
                              $ bind x $ mkApp (ann e2) e2 [Var (ann e2) Nothing tx x]
                  e -> pure e

            isLambda :: Exp -> Bool
            isLambda e = case e of
                  Lam {} -> True
                  _      -> False

            dstBind :: Exp -> Maybe (Annote, Exp, Exp)
            dstBind e = case flattenApp e of
                  [Builtin a _ _ Bind, e1, e2] -> Just (a, e1, e2)
                  _                            -> Nothing

            mkBind :: Annote -> Exp -> Exp -> Exp
            mkBind a e1 e2 = mkApp a (Builtin a Nothing (typeOf e1 `arr'` typeOf e2 `arr'` (arrowRight <$> typeOf e2)) Bind) [e1, e2]
                  where arr' :: Maybe Ty -> Maybe Ty -> Maybe Ty
                        arr' = liftM2 arr

            flatten :: (Fresh m, MonadError AstError m) => [Defn] -> Exp -> m Exp
            flatten ds = fix "Bind LHS definition expansion" 100 (pure . substs (map defnSubst ds))

            isReacDefn :: Defn -> Bool
            isReacDefn Defn { defnPolyTy = Embed (Poly (unsafeUnbind -> (_, t))) } = isReacT t

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
                        Just (TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t) _) -> do
                              x <- fresh $ s2n "$x"
                              fullyApply $ bind (vs <> [x]) $ appl t (Var (ann e') Nothing (Just t) x) e'
                        _                                                    -> pure e

            appl :: Ty -> Exp -> Exp -> Exp
            appl t x = \ case
                  Match an tan t' e1 p e Nothing    -> Match an tan (arrowRight <$> t') (mkPair an e1 x) (mkPairMPat an p $ MatchPatVar an Nothing $ Just t) e Nothing
                  Match an tan t' e1 p e (Just els) -> Match an tan (arrowRight <$> t') (mkPair an e1 x) (mkPairMPat an p $ MatchPatVar an Nothing $ Just t) e $ Just $ appl t x els
                  e                                 -> mkApp (ann e) e [x]

-- | Lifts lambdas and case/match into a top level fun def.
-- TODO(chathhorn): a lot of duplicated code here.
-- TODO(chathhorn): use Writer instead of State
liftLambdas :: Fresh m => FreeProgram -> m FreeProgram
liftLambdas p = evalStateT (liftLambdas' p) []
      where liftLambdas' :: (MonadState [Defn] m, Fresh m) => FreeProgram -> m FreeProgram
            liftLambdas' =  transformM (\ case
                  Lam an tan (Just t) b | Just te <- typeOf $ snd $ unsafeUnbind b-> do
                        (x, e)    <- unbind b
                        -- The only free names should be globally-bound
                        -- variables (and x) at this point and we can also
                        -- assume every bound name in e' was bound at a level above e'.
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr arr te $ map snd bvs <> [t]
                        f     <- fresh $ s2n "$LL.lambda"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind (fvs <> [x]) e')
                        pure $ setTyAnn tan $ mkApp an (Var an Nothing (Just t') f) $ map (toVar an . first promote) bvs
                  Case an tan t e1 b e2 | Just te <- typeOf $ snd $ unsafeUnbind b -> do
                        (p, b')    <- unbind b
                        let lvars = toVar an . first promote <$> bv b'

                        b''   <- case b' of
                              Var _ _ _ n | isFreeName n, n `notElem` (fst <$> patVars p)
                                    -> pure b'
                              _     -> do
                                    (fvs, body) <- freshen b'
                                    f           <- fresh $ s2n "$LL.case"
                                    let t'       = foldr arr te $ (snd <$> patVars p) <> map snd (bv b')
                                    modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind ((fst <$> patVars p) <> fvs) body)
                                    pure $ Var an Nothing (Just t') f

                        let (disc, pat) | null lvars = (e1, transPat p)
                                        | otherwise  = ( mkTuple an $ [e1] <> lvars
                                                       , mkTupleMPat an $ [transPat p] <> map (MatchPatVar an Nothing . typeOf) lvars
                                                       )

                        pure $ Match an tan t disc pat b'' e2
                  -- TODO(chathhorn): This case is really just normalizing Match, consider moving to ToCore.
                  Match an tan t e1 p e els | Just te <- typeOf e, liftable e -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) te bvs
                        f     <- fresh $ s2n "$LL.match"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        let lvars = map (toVar an) bvs
                        pure $ Match an tan t (mkTuple an $ [e1] <> lvars)
                                    (mkTupleMPat an $ [p] <> map (MatchPatVar an Nothing . typeOf) lvars)
                                    (Var an Nothing (Just t') f) els
                  -- Lifts matches in the operator position of an application.
                  -- TODO(chathhorn): move somewhere else?
                  App an tan _ e@Match {} arg | Just te <- typeOf e -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) te bvs
                        f     <- fresh $ s2n "$LL.matchApp"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        pure $ setTyAnn tan $ mkApp an (Var an Nothing (Just t') f) $ map (toVar an) bvs <> [arg]
                  -- Lifts the first argument to extrude (required for purification).
                  App an tan t ex@(Builtin _ _ _ Extrude) e | Just te <- typeOf e, liftable e, not $ isExtrude e -> do
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (arr . snd) te bvs
                        f     <- fresh $ s2n "$LL.extrudeApp"

                        modify $ (:) $ Defn an f (fv t' |-> t') Nothing (Embed $ bind fvs e')
                        pure $ App an tan t ex (mkApp an (Var an Nothing (Just t') f) $ map (toVar an) bvs)
                  e -> pure e)
                 >=> (\ (ts, syns, ds) -> (ts, syns,) <$> gets (ds <>))


            isExtrude :: Exp -> Bool
            isExtrude e = case flattenApp e of
                  [Builtin _ _ _ Extrude, _, _] -> True
                  _                             -> False

            liftable :: Exp -> Bool
            liftable e = case e of
                  Var {} -> False
                  _      -> True

            toVar :: Annote -> (Name Exp, Ty) -> Exp
            toVar an (v, vt) = Var an Nothing (Just vt) v

            freshen :: Fresh m => Exp -> m ([Name Exp], Exp)
            freshen e = do
                  fvs      <- replicateM (length $ bv e) $ fresh $ s2n "$LL"
                  pure (fvs, substs' (zip (fst <$> bv e) fvs) e)

            substs' :: [(Name Exp, Name Exp)] -> Exp -> Exp
            substs' subs = transform (\ n -> fromMaybe n (lookup n subs))

            -- | Get well-typed(!) bound variables.
            bv :: Data a => a -> [(Name Exp, Ty)]
            bv a = nubOrdOn fst [(n, t) | Var _ _ (Just t) n <- query a, not $ isFreeName n]

            -- | Get well-typed(!) pat variables.
            patVars :: Pat -> [(Name Exp, Ty)]
            patVars = \ case
                  PatCon _ _ _ _ ps             -> concatMap patVars ps
                  PatVar _ _ (Embed (Just t)) x -> [(x, t)]
                  _                             -> []

            promote :: Name a -> Name a
            promote = \ case
                  Bn l k | l >= 0 -> Bn (l - 1) k
                  b               -> b

            transPat :: Pat -> MatchPat
            transPat = \ case
                  PatCon an (Embed tan) (Embed t) (Embed c) ps -> MatchPatCon an tan t c $ map transPat ps
                  PatVar an (Embed tan) (Embed t) _            -> MatchPatVar an tan t
                  PatWildCard an (Embed tan) (Embed t)         -> MatchPatWildCard an tan t

-- | Remove all definitions unused by those in the given list.
purgeUnused :: [Text] -> FreeProgram -> FreeProgram
purgeUnused except (ts, syns, vs) = (inuseData (fix' extendWithCtorParams $ externCtors vs') (fv $ trec vs') ts, syns, vs')
      where vs' :: [Defn]
            vs' = inuseDefn except vs

            inuseDefn :: [Text] -> [Defn] -> [Defn]
            inuseDefn except ds = map toDefn $ Set.elems $ execState (inuseDefn' ds') ds'
                  where inuseDefn' :: MonadState (Set (Name Exp)) m => Set (Name Exp) -> m ()
                        inuseDefn' ns | Set.null ns = pure () -- TODO(chathhorn): rewrite using fix?
                                      | otherwise   = do
                              inuse  <- get
                              modify $ union $ fvs ns
                              inuse' <- get
                              inuseDefn' $ inuse' \\ inuse

                        ds' :: Set (Name Exp)
                        ds' = Set.fromList $ filter (flip elem except . n2s) $ map defnName ds

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
                         , "Bool"
                         ]

            -- | Also treat as used: all ctors for types returned by externs and ReacT inputs.
            externCtors :: Data a => a -> [Name TyConId]
            externCtors a = concat $ [maybe [] (ctorNames . rangeTy) $ typeOf e | e@Builtin {} <- query a]
                  <> [maybe [] ctorNames $ resInputTy t | Defn _ (n2s -> n) (Embed (Poly (unsafeUnbind -> (_, t)))) _ _ <- query a, n `elem` except]

            extendWithCtorParams :: [Name TyConId] -> [Name TyConId]
            extendWithCtorParams = nubOrd . sort . foldr extend' []
                  where extend' :: Name TyConId -> [Name TyConId] -> [Name TyConId]
                        extend' n = (<> concatMap (concatMap ctorTypes . dataCons) (filter ((== n2s n) . n2s . dataName) ts))

                        ctorTypes :: DataCon -> [Name TyConId]
                        ctorTypes (DataCon _ _ (Embed (Poly (unsafeUnbind -> (_, t))))) = ctorNames t

            dataConName :: DataCon -> Name DataConId
            dataConName (DataCon _ n _) = n

-- | Repeatedly calls "reduce" and "specialize" -- attempts to remove
-- higher-order functions by partially evaluating them.
simplify :: (Fresh m, MonadError AstError m) => Config -> FreeProgram -> m FreeProgram
simplify conf = flip evalStateT mempty . boundedFix tst (conf^.depth) (specialize >=> reduce)
      where tst :: FreeProgram -> FreeProgram -> Bool
            tst (_, _, vs) (_, _, vs') = hash (unAnn vs) == hash (unAnn vs')

type SpecMap = HashMap (Name Exp, AppSig) Defn
type AppSig = [Maybe Exp]

-- | Replaces all free type variables with "()". We presume polymorphic
--   arguments that haven't been inferred to have a more concrete type,
--   must be unused.
freeTyVarsToNil :: FreeProgram -> FreeProgram
freeTyVarsToNil (ts, syns, vs) = (ts, syns, map upd vs)
      where upd :: Defn -> Defn
            upd d@Defn
                  { defnPolyTy = Embed (Poly (unsafeUnbind -> (_, t)))
                  , defnBody   = Embed b
                  } = d { defnPolyTy = Embed $ poly [] $ sub t
                        , defnBody   = Embed $ sub b
                        }

            sub :: (Alpha a, Subst Ty a) => a -> a
            sub v = substs (map (, nilTy) $ nubOrd $ fv v) v

-- | If b only has global variables (not lambda-bound), then
-- > f :: A -> X
-- > f = \ a -> g a b
-- > g :: A -> B -> X
-- > g = g_rhs
--   becomes
-- > f :: A -> X
-- > f = \ a -> g' a
-- > g :: A -> B -> X
-- > g = g_rhs
-- > g' :: A -> X
-- > g' = \ a' -> g_rhs a' b
specialize :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => FreeProgram -> m FreeProgram
specialize (ts, syns, vs) = do
      vs'     <- mapM specDefn vs
      newDefs <- gets $ filter isNewDefn . Map.elems
      pure (ts, syns, vs' <> newDefs)
      where gs :: HashMap (Name Exp) Defn
            gs = Map.fromList $ map (defnName &&& id) vs

            isNewDefn :: Defn -> Bool
            isNewDefn = not . isGlobal . defnName

            isGlobal :: Name Exp -> Bool
            isGlobal = flip Map.member gs

            specDefn :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => Defn -> m Defn
            specDefn (Defn ann n pt inl (Embed body)) = do
                  (vs, body') <- unbind body
                  Defn ann n pt inl <$> Embed <$> bind vs <$> specExp body'

            specExp :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => Exp -> m Exp
            specExp = \ case
                  e@(App an tan t e' a') | Var _ _ _ g : args <- flattenApp e
                                         , Just d             <- Map.lookup g gs
                                         , inlineable d
                                               -> do
                        args' <- mapM specExp args
                        let s = sig args'
                        if | all isNothing s -> App an tan t <$> specExp e' <*> specExp a'
                           | otherwise       -> do
                              d'   <- gets (Map.lookup (g, s)) >>= \ case
                                    Just d'' -> pure d''
                                    _        -> do
                                          d'' <- mkDefn s d
                                          modify $ Map.insert (g, s) d''
                                          pure d''
                              t'   <- getTy d'
                              pure $ mkGApp an (defnName d') t' args' s
                  App an tan t e arg               -> App an tan t <$> specExp e <*> specExp arg
                  Lam an tan t b                   -> do
                        (vs, b') <- unbind b
                        Lam an tan t . bind vs <$> specExp b'
                  Case an tan t e p (Just e')      -> do
                        (ps, p') <- unbind p
                        Case an tan t <$> specExp e <*> (bind ps <$> specExp p') <*> (Just <$> specExp e')
                  Case an tan t e p Nothing        -> do
                        (ps, p') <- unbind p
                        Case an tan t <$> specExp e <*> (bind ps <$> specExp p') <*> pure Nothing
                  Match an tan t e p e' (Just e'') -> Match an tan t <$> specExp e <*> pure p <*> specExp e' <*> (Just <$> specExp e'')
                  Match an tan t e p e' Nothing    -> Match an tan t <$> specExp e <*> pure p <*> specExp e' <*> pure Nothing
                  LitList an tan t es              -> LitList an tan t <$> mapM specExp es
                  LitVec an tan t es               -> LitVec an tan t <$> mapM specExp es
                  e@LitInt {}                      -> pure e
                  e@LitStr {}                      -> pure e
                  e@Var {}                         -> pure e
                  e@Con {}                         -> pure e
                  e@Builtin {}                     -> pure e

            sig :: [Exp] -> AppSig
            sig = map $ \ e -> if | all isGlobal $ fv e -> Just $ unAnn e
                                  | otherwise           -> Nothing

            getTy :: Fresh m => Defn -> m Ty
            getTy Defn { defnPolyTy = Embed (Poly pt) } = snd <$> unbind pt

            mkDefn :: (MonadError AstError m, Fresh m) => AppSig -> Defn -> m Defn
            mkDefn s (Defn an g (Embed (Poly bgt)) inl (Embed body)) = do
                  g'              <- fresh g
                  gt              <- snd <$> unbind bgt
                  (tinfo, t')     <- mkTy s gt
                  (bodyvs, body') <- unbind body
                  typeCheckDefn ts vs
                       $ Defn an g' (Embed $ Poly $ bind (fv t') t') inl
                       $ Embed $ bind []
                               $ mkLam an (lefts tinfo)
                               $ mkApp an (mkLam an (zip (fst $ flattenArrow gt) bodyvs) (setTyAnn (Just $ Poly bgt) body'))
                               $ map (either (uncurry $ Var an Nothing . Just) id) tinfo

            mkTy :: (Fresh m, MonadError AstError m) => AppSig -> Ty -> m ([Either (Ty, Name Exp) Exp], Ty)
            mkTy s (flattenArrow -> (gtas, gtr)) = do
                  (gtas', subs) <- runStateT (zipWithM mkTy' gtas s) mempty
                  pure $ substs (Map.toList subs) (gtas', foldr arr gtr $ map fst (lefts gtas') <> drop (length gtas') gtas)
                  where mkTy' :: (Fresh m, MonadState TySub m, MonadError AstError m) => Ty -> Maybe Exp -> m (Either (Ty, Name Exp) Exp)
                        mkTy' t = \ case
                              Just e | Just te <- typeOf e -> do
                                    t' <- unify (ann e) t te
                                    pure $ Right $ setTyAnn (Just $ poly' t') e
                              Just e -> pure $ Right $ setTyAnn (Just $ poly' t) e
                              Nothing -> do
                                    n <- fresh $ s2n "sp"
                                    pure $ Left (t, n)

            mkGApp :: Annote -> Name Exp -> Ty -> [Exp] -> AppSig -> Exp
            mkGApp an g t es = mkApp an (Var an Nothing (Just t) g) . catMaybes . zipWith mkGApp' es
                  where mkGApp' :: Exp -> Maybe Exp -> Maybe Exp
                        mkGApp' e = maybe (Just e) (const Nothing)

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
                  App an tan t e1 e2      -> do
                        e1' <- reduceExp e1
                        e2' <- reduceExp e2
                        case e1' of
                              Lam _ _ tx e -> do
                                    (x, e') <- unbind e
                                    beta <- reduceExp $ subst x (setTyAnn (poly' <$> tx) e2') e' -- TODO(chathhorn): ad-hoc promoting current type to annotation.
                                    pure $ setTyAnn tan beta
                              _              -> pure $ App an tan t e1' e2'
                  Lam an tan t e        -> do
                        (x, e') <- unbind e
                        Lam an tan t . bind x <$> reduceExp e'
                  Case an tan t e e1 e2 -> do
                        (p, e1') <- unbind e1
                        e'       <- reduceExp e
                        case matchPat e' p of
                              MatchYes sub -> reduceExp $ substs sub e1'
                              MatchMaybe   -> case e2 of
                                    Nothing  -> Case an tan t e' <$> (bind p <$> reduceExp e1') <*> pure Nothing
                                    Just e2' -> -- trace ("MAYBEmatch " ++ show (pretty e')
                                                --  ++ "\nof " ++ show (pretty p)) $
                                                Case an tan t e' <$> (bind p <$> reduceExp e1') <*> (Just <$> reduceExp e2')
                              MatchNo      -> case e2 of
                                    Nothing  -> -- trace ("ERRORmatch " ++ show (pretty e')
                                                --    ++ "\nof " ++ show (pretty p)) $ 
                                                pure $ mkError an t "Pattern match failure (reduced)"
                                    Just e2' -> -- trace ("NOmatch " ++ show (pretty e')
                                                --    ++ "\nof " ++ show (pretty p)) $
                                                reduceExp e2'
                  -- TODO(chathhorn): handle match?
                  Match an tan t e p e' Nothing    -> Match an tan t <$> reduceExp e <*> pure p <*> reduceExp e' <*> pure Nothing
                  Match an tan t e p e' (Just e'') -> Match an tan t <$> reduceExp e <*> pure p <*> reduceExp e' <*> (Just <$> reduceExp e'')
                  LitList an tan t es              -> LitList an tan t <$> mapM reduceExp es
                  LitVec an tan t es               -> LitVec an tan t <$> mapM reduceExp es
                  e@LitInt {}                      -> pure e
                  e@LitStr {}                      -> pure e
                  e@Var {}                         -> pure e
                  e@Con {}                         -> pure e
                  e@Builtin {}                     -> pure e

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
                  PatCon _ _ _ (Embed i) pats -> case flattenApp e of
                        Con _ _ _ c : es
                              | c == i && length es == length pats -> mergeMatches $ zipWith matchPat es pats
                              | otherwise                          -> MatchNo
                        _                                          -> MatchMaybe
                  PatVar _ _ _ x              -> MatchYes [(x, e)]
                  PatWildCard {}              -> MatchYes []
