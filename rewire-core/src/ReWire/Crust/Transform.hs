{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReWire.Crust.Transform
      ( inlineAnnotated, inlineExtrudes
      , expandTypeSynonyms, reduce
      , neuterExterns
      , shiftLambdas
      , unshiftLambdas
      , liftLambdas
      , etaAbsDefs
      , purge, purgeAll
      , normalizeBind
      , elimCase
      , simplify
      , specialize
      , freeTyVarsToNil
      , removeMain
      ) where

import ReWire.Annotation (Annote (..), Annotated (..), unAnn)
import ReWire.Config (Config, depth, start, pDebug)
import ReWire.Crust.PrimBasis (primDatas)
import ReWire.Crust.Syntax (Exp (..), Kind (..), Ty (..), Poly (..), Pat (..), MatchPat (..), Defn (..), FreeProgram, DataCon (..), DataConId, TyConId, DataDefn (..), Builtin (..), DefnAttr (..), TypeSynonym (..), flattenApp, builtins)
import ReWire.Crust.TypeCheck (typeCheckDefn, unify, unify', TySub)
import ReWire.Crust.Types (typeOf, tyAnn, setTyAnn, dstArrow, maybeSetTyAnn, poly, poly', flattenArrow, arr, nilTy, ctorNames, resInputTy, codomTy, (|->), arrowRight, arrowLeft, isReacT, prettyTy, synthable)
import ReWire.Crust.Util (mkApp, mkError, mkLam, inlinable, synthableDefn, mkTupleMPat, mkTuple, mkPairMPat, mkPair, patVars, toVar, transPat, transMPat, isExtrude, extrudeDefn)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fix, fix', fixUntil)
import ReWire.SYB (transform, transformM, query)
import ReWire.Unbound (freshVar, fv, Fresh (fresh), s2n, n2s, substs, subst, unembed, isFreeName, runFreshM, Name (..), unsafeUnbind, bind, unbind, Subst (..), Alpha, Embed (Embed), Bind, trec)

import Control.Arrow ((&&&))
import Control.Lens ((^.))
import Control.Monad (liftM2, foldM, foldM_, zipWithM, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, evalStateT, execState, StateT (..), get, gets, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data)
import Data.Either (lefts)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet, union, difference)
import Data.Hashable (Hashable)
import Data.List (find, sort)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text, isPrefixOf)
import Data.Tuple (swap)

import qualified Data.Text           as Text
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- | Removes the Main.main function definition, which is unused by rwc.
removeMain :: FreeProgram -> FreeProgram
removeMain (ts, syns, ds) = (ts, syns, filter (not . isMain) ds)
      where isMain :: Defn -> Bool
            isMain = (== "Main.main") . n2s . defnName

-- | Inlines defs marked for inlining.
inlineAnnotated :: MonadError AstError m => FreeProgram -> m FreeProgram
inlineAnnotated (ts, syns, ds) = (ts, syns,) <$> (substs <$> subs <*> pure ds)
      where inlineDefs :: [Defn]
            inlineDefs = filter isInline ds

            subs :: MonadError AstError m => m [(Name Exp, Exp)]
            subs = map defnSubst <$> ifix (pure . substs (map defnSubst inlineDefs)) inlineDefs

            ifix :: (Hashable a, MonadError AstError m) => (a -> m a) -> a -> m a
            ifix = fix "INLINE definition expansion" 500

            isInline :: Defn -> Bool
            isInline d = defnAttr d == Just Inline

-- | Inlines defs that use the "extrude" primitive for purification.
inlineExtrudes :: MonadError AstError m => FreeProgram -> m FreeProgram
inlineExtrudes (ts, syns, ds) = (ts, syns,) <$> inlineExtrudes ds
      where inlineExtrudes :: MonadError AstError m => [Defn] -> m [Defn]
            inlineExtrudes = ifix (pure . inlineExtrudes')

            inlineExtrudes' :: [Defn] -> [Defn]
            inlineExtrudes' ds' = substs (map defnSubst $ filter extrudeDefn ds') ds'

            ifix :: (Hashable a, MonadError AstError m) => (a -> m a) -> a -> m a
            ifix = fix "Inlining definitions that use 'extrude'" 100


defnSubst :: Defn -> (Name Exp, Exp)
defnSubst (Defn _ n (Embed pt) _ (Embed e)) = runFreshM $ unbind e >>= \ case
      ([], e') -> pure (n, setTyAnn (Just pt) e')
      _        -> error $ "Inlining: definition not inlinable (rwc bug): " <> show n

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
                        pure $ if null vs then t' else t
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
                  (Builtin _ _ _ Extern, [_, _, _, _, _, _]) -> True
                  _                                          -> False

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

-- | Shifts vars bound by top-level lambdas into defs.
-- > g x1 x2 = e
--   becomes
-- > g = \ x1 -> \ x2 -> e
unshiftLambdas :: Fresh m => FreeProgram -> m FreeProgram
unshiftLambdas (ts, syns, vs) = (ts, syns, ) <$> mapM unshiftLambdas' vs
      where unshiftLambdas' :: Fresh m => Defn -> m Defn
            unshiftLambdas' (Defn an n (Embed (Poly t)) inl (Embed e)) = Defn an n (Embed $ Poly t) inl . Embed <$> do
                  (vs, e') <- unbind e
                  (_, t')  <- unbind t
                  let (tvs, _) = flattenArrow t'
                  pure $ bind [] $ mkLam an (zip tvs vs) e'

-- | Inlines everything to the left of ">>=" and
-- > (m >>= f) >>= g
-- > (m >>= (\ x -> s)) >>= g
-- becomes
-- > m >>= (\ x -> f x >>= g)
-- > m >>= (\ x -> s >>= g)
-- also
-- > m >>= f
-- becomes
-- > m >>= \ x -> f x
-- and bind RHS is then lambda-lifted. Also lifts to a global definition:
-- - the first argument to `extrude` to a global definition.
-- - any `match` expression that appears in the operator position of an application.
normalizeBind :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
normalizeBind (ts, syns, ds) = (ts, syns, ) <$> (uncurry (<>) <$> runStateT (mapM ppDefn ds) [])
      where ppDefn :: (MonadState [Defn] m, MonadError AstError m, Fresh m) => Defn -> m Defn
            ppDefn d@Defn { defnName = dn, defnBody = Embed e } = do
                  (xs, e') <- unbind e
                  e''      <- ppExp (n2s dn) (Set.fromList xs) e'
                  pure $ d { defnBody = Embed (bind xs e'') }

            ppExp :: (MonadState [Defn] m, MonadError AstError m, Fresh m) => Text -> HashSet (Name Exp) -> Exp -> m Exp
            ppExp dn bvs = \ case
                  -- Bind: associate to the right (case with a lambda on the right).
                  -- > (ma >>= \ a -> mb) >>= fb
                  -- becomes
                  -- > ma >>= (\ a -> (mb >>= fb))
                  (dstBind -> Just (a, dstBind -> Just (_, e1, Lam _ _ tx e2), e3)) -> do
                        -- TODO(chathhorn): note eliding type annotation.
                        (x, e2') <- unbind e2
                        ppExp' $ mkBind a e1 $ Lam (ann e2') Nothing tx $ bind x $ mkBind a e2' e3

                  -- Bind: associate to the right.
                  -- > (m >>= fa) >>= fb
                  -- becomes
                  -- > m >>= (\ x -> fa x >>= fb)
                  (dstBind -> Just (a, dstBind -> Just (_, e1, e2), e3)) -> do
                        let tx = arrowLeft <$> typeOf e2
                        x <- freshVar "rabind"
                        let e' = mkBind a (mkApp (ann e2) e2 [Var (ann e2) Nothing tx x]) e3
                        ppExp' $ mkBind a e1 $ Lam (ann e2) Nothing tx (bind x e')

                  -- Bind: if Match is on the LHS, push the bind into the
                  -- Match arms.
                  -- > (match disc p e els) >>= f
                  -- becomes
                  -- > match disc p e' (els >>= f)
                  -- > where e' = e >>= f
                  -- if e has a non-function type, otherwise
                  -- >       e' = \ x1... -> (e x1... >>= f)
                  b@(dstBind -> Just (a, Match an tan _ disc p e els, e2)) -> do
                        pvs <- patVars <$> transMPat p
                        let e'   = mkLam an pvs $ mkBind a (mkApp an (maybeSetTyAnn tan e) (toVar an <$> pvs)) e2
                            els' = flip (mkBind a) e2 . maybeSetTyAnn tan <$> els
                        ppExp' $ Match an Nothing (typeOf b) disc p e' els'

                  -- Bind: inline everything on the LHS, lift the RHS.
                  (dstBind -> Just (a, e1, e2)) -> do
                        e2'   <- ppExp' e2 >>= lift' -- lift RHS

                        lams  <- get
                        e1'   <- ppExp' e1
                                 >>= flatten (filter (\ d -> isReacDefn d && inlinable d) $ ds <> lams)
                                 >>= reduceExp

                        let b = mkBind a e1' e2'
                        if leftAssocBind b || matchBind b then ppExp' b
                        else pure b

                  -- Lifts matches in the operator position of an application (for toCore and purification).
                  App an tan t e@Match {} arg                                   -> App an tan t <$> (ppExp' e >>= lift') <*> ppExp' arg
                  -- Lifts the first argument to extrude (for purification).
                  App an tan t ex@(Builtin _ _ _ Extrude) e | not $ isExtrude e -> App an tan t ex <$> (ppExp' e >>= lift')
                  App an tan t e1 e2 -> App an tan t <$> ppExp' e1 <*> ppExp' e2
                  Lam an tan t  e -> do
                        (x, e') <- unbind e
                        Lam an tan t . bind x <$> ppExp dn (Set.insert x bvs) e'
                  Case an tan t disc e els -> do
                        (p, e') <- unbind e
                        Case an tan t <$> ppExp' disc <*> (bind p <$> ppExp dn (Set.fromList (snd <$> patVars p) <> bvs) e') <*> mapM ppExp'  els
                  Match an tan t disc p e els -> Match an tan t <$> ppExp' disc <*> pure p <*> ppExp' e <*> mapM ppExp' els
                  LitList an tan t es         -> LitList an tan t <$> mapM ppExp' es
                  LitVec an tan t es          -> LitVec an tan t <$> mapM ppExp' es
                  e                           -> pure e

                  where ppExp' :: (MonadState [Defn] m, MonadError AstError m, Fresh m) => Exp -> m Exp
                        ppExp' = ppExp dn bvs

                        lift' :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Exp -> m Exp
                        lift' = lift dn bvs

            leftAssocBind :: Exp -> Bool
            leftAssocBind = \ case
                  (dstBind -> Just (_, dstBind -> Just (_, _, _), _)) -> True
                  _                                                   -> False

            matchBind :: Exp -> Bool
            matchBind = \ case
                  (dstBind -> Just (_, Match {}, _)) -> True
                  _                                  -> False

            dstBind :: Exp -> Maybe (Annote, Exp, Exp)
            dstBind e = case flattenApp e of
                  (Builtin a _ _ Bind, [e1, e2]) -> Just (a, e1, e2)
                  _                              -> Nothing

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
etaAbsDefs :: Fresh m => FreeProgram -> m FreeProgram
etaAbsDefs (ts, syns, vs) = (ts, syns, ) <$> mapM etaAbsDefs' vs
      where etaAbsDefs' :: Fresh m => Defn -> m Defn
            etaAbsDefs' (Defn an n t inl (Embed e)) = Defn an n t inl . Embed <$> etaAbs e

            etaAbs :: Fresh m => Bind [Name Exp] Exp -> m (Bind [Name Exp] Exp)
            etaAbs e = do
                  (vs, e') <- unbind e
                  case typeOf e' of
                        Just (dstArrow -> Just (t, _)) -> do
                              x <- freshVar "$x"
                              etaAbs $ bind (vs <> [x]) $ appl t (Var (ann e') Nothing (Just t) x) e'
                        _                                                    -> pure e

            appl :: Ty -> Exp -> Exp -> Exp
            appl t x = \ case
                  Match an tan t' e1 p e els -> Match an tan (arrowRight <$> t') (mkPair an e1 x) (mkPairMPat an p $ MatchPatVar an Nothing $ Just t) e $ appl t x <$> els
                  e                          -> mkApp (ann e) e [x]

elimCase :: Fresh m => FreeProgram -> m FreeProgram
elimCase (ts, syns, ds) = (ts, syns,) <$> mapM ecDefn ds
      where ecDefn :: Fresh m => Defn -> m Defn
            ecDefn d@Defn { defnBody = Embed e } = do
                  (bvs, e') <- unbind e
                  e''       <- ecExp e'
                  pure $ d { defnBody = Embed (bind bvs e'') }

            ecExp :: Fresh m => Exp -> m Exp
            ecExp = \ case
                  Case an tan t disc b els -> do
                        (p, b') <- unbind b
                        Match an tan t <$> ecExp disc <*> pure (transPat p) <*> (mkLam an (patVars p) <$> ecExp b') <*> mapM ecExp els
                  Match an tan t disc p e els -> Match an tan t <$> ecExp disc <*> pure p <*> ecExp e <*> mapM ecExp els
                  Lam an tan t b           -> do
                        (x, b') <- unbind b
                        Lam an tan t . bind x <$> ecExp b'
                  App an tan t e1 e2       -> App an tan t <$> ecExp e1 <*> ecExp e2
                  LitVec an tan t es       -> LitVec an tan t <$> mapM ecExp es
                  LitList an tan t es      -> LitList an tan t <$> mapM ecExp es
                  e                        -> pure e

-- | Lifts lambdas and case/match into a top level fun def.
--   E.g., lifts
--   `\ x -> \y -> ... -> e`
--   to a global definition
--   `g = \ x -> \ y -> ... -> e`
--   Doesn't lift anything with a higher-order type.
liftLambdas :: (Fresh m, MonadError AstError m) => FreeProgram -> m FreeProgram
liftLambdas (ts, syns, ds) = (ts, syns,) . uncurry (<>) <$> runStateT (mapM llDefn ds) []

-- | Don't lambda-lift initial lambdas on RHS of top-level binding.
llDefn :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Defn -> m Defn
llDefn d@Defn { defnName = dn, defnBody = Embed e } = do
      (bvs, e') <- unbind e
      e''       <- llDefnBody (n2s dn) (Set.fromList bvs) e'
      pure $ d { defnBody = Embed (bind bvs e'') }

      where llDefnBody :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Text -> HashSet (Name Exp) -> Exp -> m Exp
            llDefnBody dn bvs = \ case
                  Lam an pt t e -> do
                        (v, e') <- unbind e
                        e''     <- llDefnBody dn (Set.insert v bvs) e'
                        pure $ Lam an pt t $ bind v e''
                  e             -> llExp dn bvs e

llExp :: (MonadState [Defn] m, MonadError AstError m, Fresh m) => Text -> HashSet (Name Exp) -> Exp -> m Exp
llExp dn bvs =  \ case
      Lam an tan t b -> do
            (x, b') <- unbind b
            case b' of
                  -- Eta-reduce instead of lift (duplicated from reduceExp).
                  App _ _ _ e1 (Var _ _ _ x')
                        | not (isBuiltin e1) -- TODO: ugly special case to accomodate purify.
                        , x == x'
                        , x `notElem` fv e1 -> llExp' e1
                  _                         -> do
                        b'' <- llExp dn (Set.insert x bvs) b'
                        lift' $ Lam an tan t $ bind x b''

      -- Lifts anything other than a Var in the first Match branch. This case
      -- is really just normalizing Match (TODO: consider moving to ToCore?).
      Match an tan t disc p e els -> do
            e' <- llExp' e >>= lift'
            let (v, lvars) = flattenApp e'
            Match an tan t
                  <$> llExp' (mkTuple an $ lvars <> [disc])
                  <*> pure (mkTupleMPat an $ (MatchPatVar an Nothing . typeOf <$> lvars) <> [p])
                  <*> pure v
                  <*> mapM llExp' els
      App an tan t e1 e2  -> App an tan t <$> llExp' e1 <*> llExp' e2
      LitList an tan t es -> LitList an tan t <$> mapM llExp' es
      LitVec an tan t es  -> LitVec an tan t <$> mapM llExp' es
      e                   -> pure e

      where llExp' :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Exp -> m Exp
            llExp' = llExp dn bvs

            lift' :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Exp -> m Exp
            lift' = lift dn bvs

            isBuiltin :: Exp -> Bool
            isBuiltin = \ case
                  Builtin {} -> True
                  _          -> False

-- | Lifts an expression to a definition and returns an application (to pass
--   any free variables). Argument is a list of non-global free variables.
lift :: (Fresh m, MonadState [Defn] m, MonadError AstError m) => Text -> HashSet (Name Exp) -> Exp -> m Exp
lift dn bvs e | Just t  <- typeOf e
              , not $ isGlobal e = do
      fvs    <- filter ((`Set.member` bvs) . snd) <$> freevars e
      let t'  = foldr arr t $ fst <$> fvs
          an  = ann e

      if not $ synthable t' then pure e else do
            f      <- freshVar $ prefix dn
            d      <- llDefn $ Defn an f (fv t' |-> t') Nothing $ Embed $ bind [] $ mkLam an fvs e
            modify (d :)
            pure $ setTyAnn (tyAnn e) $ mkApp an (Var an Nothing (Just t') f) $ toVar an <$> fvs

      where -- | Get well-typed free variables.
            freevars :: (MonadError AstError m, Data a) => a -> m [(Ty, Name Exp)]
            freevars a = map swap . Map.toList <$> mapM (foldM unifyVs (TyVar an KStar $ s2n "?lift")) fvs'
                  where fvs' :: HashMap (Name Exp) [Maybe Ty]
                        fvs' = Map.fromListWith (<>) [(n, [t]) | Var _ _ t n <- query a, isFreeName n]

                        unifyVs :: MonadError AstError m => Ty -> Maybe Ty -> m Ty
                        unifyVs t1 = \ case
                              Nothing -> pure t1
                              Just t2 -> maybe (failAt (ann t1) $ "Mis-typed variables; could not unify: " <> prettyTy t1 <> " with " <> prettyTy t2) pure
                                       $ unify' t1 t2

                        an :: Annote
                        an = MsgAnnote "Lift: dummy type variable"

            isGlobal :: Exp -> Bool
            isGlobal = \ case
                  Var _ _ _ n -> not (n `Set.member` bvs)
                  _           -> False

            prefix :: Text -> Text
            prefix x | pre `isPrefixOf` x = x
                     | "$" `isPrefixOf` x = pre <> Text.drop 1 x
                     | otherwise          = pre <> x

            pre :: Text
            pre = "$LL."
lift _ _ e                                            = pure e

purge :: Applicative m => Name Exp -> FreeProgram -> m FreeProgram
purge start = pure . purgeUnused (start : (s2n . fst <$> builtins)) (dataName <$> primDatas)

purgeAll :: Applicative m => Name Exp -> FreeProgram -> m FreeProgram
purgeAll start = pure . purgeUnused [start] []

-- | Remove all definitions and types unused by those in the given lists.
purgeUnused :: [Name Exp] -> [Name TyConId] -> FreeProgram -> FreeProgram
purgeUnused except exceptTs (ts, syns, vs) = (inuseData (fix' extendWithCtorParams $ externCtors vs') (fv $ trec vs') ts, syns, vs')
      where vs' :: [Defn]
            vs' = inuseDefn except vs

            inuseDefn :: [Name Exp] -> [Defn] -> [Defn]
            inuseDefn except ds = map toDefn $ Set.toList $ execState (inuseDefn' ds') ds'
                  where inuseDefn' :: MonadState (HashSet (Name Exp)) m => HashSet (Name Exp) -> m ()
                        inuseDefn' ns | Set.null ns = pure () -- TODO(chathhorn): rewrite using fix?
                                      | otherwise   = do
                              inuse  <- get
                              modify $ union $ fvs ns
                              inuse' <- get
                              inuseDefn' $ inuse' `difference` inuse

                        ds' :: HashSet (Name Exp)
                        ds' = Set.fromList $ filter (`elem` except) $ map defnName ds

                        fvs :: HashSet (Name Exp) -> HashSet (Name Exp)
                        fvs = Set.fromList . concatMap (fv . unembed . defnBody . toDefn) . Set.toList

                        toDefn :: Name Exp -> Defn
                        toDefn n | Just d <- find ((== n) . defnName) ds = d
                                 | otherwise                             = error $ "Something went wrong: can't find symbol: " <> show n

            inuseData :: [Name TyConId] -> [Name DataConId] -> [DataDefn] -> [DataDefn]
            inuseData ts ns = filter (\ DataDefn {dataName = n, dataCons = cs} -> not (null cs) || n `elem` exceptTs)
                            . map (inuseData' ts ns)

            inuseData' :: [Name TyConId] -> [Name DataConId] -> DataDefn -> DataDefn
            inuseData' ts ns d@(DataDefn an n k cs)
                  | n `elem` ts       = d
                  | n `elem` exceptTs = d
                  | otherwise         = DataDefn an n k $ filter ((`Set.member` Set.fromList ns) . dataConName) cs

            -- | Also treat as used: all ctors for types returned by externs and ReacT inputs.
            externCtors :: Data a => a -> [Name TyConId]
            externCtors a = concat $ [maybe [] (ctorNames . codomTy) $ typeOf e | e@Builtin {} <- query a]
                  <> [maybe [] ctorNames $ resInputTy t | Defn _ n (Embed (Poly (unsafeUnbind -> (_, t)))) _ _ <- query a, n `elem` except]

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
simplify :: (MonadIO m, Fresh m, MonadError AstError m) => Config -> FreeProgram -> m FreeProgram
simplify conf = flip evalStateT mempty . fixUntil (\(_, _, vs) -> all synthableDefn vs) "Partial evaluation" (conf^.depth)
                (
                verb "> Specializing..."
                >=> specialize'
                >=> verb "> Purging..."
                >=> purge (s2n $ conf^.start)
                >=> verb "> Reducing..."
                >=> reduce
                )
      where verb :: MonadIO m => Text -> a -> m a
            verb s a = pDebug conf s >> pure a

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
{- HLINT ignore "Redundant multi-way if" -}
specialize :: (MonadError AstError m, Fresh m) => FreeProgram -> m FreeProgram
specialize = flip evalStateT mempty . specialize'

specialize' :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => FreeProgram -> m FreeProgram
specialize' (ts, syns, vs) = do
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
                  (Defn ann n pt inl . Embed) . bind vs <$> specExp body'

            specExp :: (MonadError AstError m, Fresh m, MonadState SpecMap m) => Exp -> m Exp
            specExp = \ case
                  e@(App an tan t e' a') | (Var _ _ _ g, args) <- flattenApp e
                                         , Just d              <- Map.lookup g gs
                                         , inlinable d
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
                  Case an tan t e p els            -> do
                        (ps, p') <- unbind p
                        Case an tan t <$> specExp e <*> (bind ps <$> specExp p') <*> mapM specExp els
                  Match an tan t e p e' els        -> Match an tan t <$> specExp e <*> pure p <*> specExp e' <*> mapM specExp els
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
            mkDefn s _dx@(Defn an g (Embed (Poly bgt)) inl (Embed body)) = do
                  g'              <- fresh g
                  gt              <- snd <$> unbind bgt
                  (tinfo, t')     <- mkTy s gt
                  (bodyvs, body') <- unbind body
                  typeCheckDefn ts vs
                       $ Defn an g' (Embed $ Poly $ bind (fv t') t') inl
                       $ Embed $ bind []
                               $ mkLam an (lefts tinfo)
                               $ mkApp an (mkLam an (zip (fst $ flattenArrow gt) bodyvs) body')
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
                                    n <- freshVar "sp"
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
            e''     <- reduceExp e'
            -- Eta reduce.
            case e'' of
                  App _ _ _ e1 (Var _ _ _ x')
                        | x == x'
                        , x `notElem` fv e1 -> reduceExp e1
                  _                         -> pure $ Lam an tan t $ bind x e''
      Case an tan t e e1 e2 -> do
            (p, e1') <- unbind e1
            e'       <- reduceExp e
            case matchPat e' p of
                  MatchYes sub -> reduceExp $ substs sub e1'
                  MatchMaybe   -> Case an tan t e' <$> (bind p <$> reduceExp e1') <*> mapM reduceExp e2
                  MatchNo      -> maybe (pure $ mkError an t "Pattern match failure: non-exhaustive patterns in case") reduceExp e2
      Match an tan t e p e1 e2 -> do
            e'       <- reduceExp e
            case matchMPat e' p of
                  MatchYes res -> reduceExp $ setTyAnn tan $ mkApp an e1 (snd <$> res)
                  MatchMaybe   -> Match an tan t e' p <$> reduceExp e1 <*> mapM reduceExp e2
                  MatchNo      -> maybe (pure $ mkError an t "Pattern match failure: non-exhaustive patterns in case") reduceExp e2
      LitList an tan t es              -> LitList an tan t <$> mapM reduceExp es
      LitVec an tan t es               -> LitVec an tan t <$> mapM reduceExp es
      e@LitInt {}                      -> pure e
      e@LitStr {}                      -> pure e
      e@Var {}                         -> pure e
      e@Con {}                         -> pure e
      e@Builtin {}                     -> pure e

      where mergeMatches :: [MatchResult] -> MatchResult
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
                        (Con _ _ _ c, es)
                              | c == i && length es == length pats -> mergeMatches $ zipWith matchPat es pats
                              | otherwise                          -> MatchNo
                        _                                          -> MatchMaybe
                  PatVar _ _ _ x              -> MatchYes [(x, e)]
                  PatWildCard {}              -> MatchYes []

            matchMPat :: Exp -> MatchPat -> MatchResult
            matchMPat e = \ case
                  MatchPatCon _ _ _ i pats    -> case flattenApp e of
                        (Con _ _ _ c, es)
                              | c == i && length es == length pats -> mergeMatches $ zipWith matchMPat es pats
                              | otherwise                          -> MatchNo
                        _                                          -> MatchMaybe
                  MatchPatVar {}              -> MatchYes [(s2n "$matchVar", e)]
                  MatchPatWildCard {}         -> MatchYes []

