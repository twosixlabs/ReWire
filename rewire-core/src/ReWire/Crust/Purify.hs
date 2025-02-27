{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Purify (purify) where

import ReWire.Annotation (Annote (MsgAnnote, NoAnnote), ann, unAnn, noAnn)
import ReWire.Crust.Syntax (Exp (..), Kind (..), Ty (..), Pat (..), MatchPat (..), DefnAttr (..), DataConId, DataCon (..), Builtin (..), Defn (..), Poly (..), DataDefn (..), FreeProgram, flattenApp)
import ReWire.Crust.TypeCheck (unify')
import ReWire.Crust.Types (tupleTy, mkArrowTy, typeOf, arrowLeft, paramTys, isReacT, codomTy, (|->), isStateT, dstArrow, dstStateT, dstTyApp, dstReacT, nilTy)
import ReWire.Crust.Util (mkApp, mkTuplePat, mkTuple, nil, isPrim, patVars, toVar, toPatVar, transPat, transMPat, mkLam)
import ReWire.Error (failAt, MonadError, AstError)
import ReWire.Pretty (TextShow (showb, showt), fromText, prettyPrint)
import ReWire.Unbound (freshVar, Fresh, s2n, n2s, bind, Name, Embed (Embed), unbind)

import Control.Arrow (first, second, (&&&))
import Control.Monad ((>=>))
import Control.Monad.State
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.List (find, isSuffixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Set (Set, singleton, insert)
import Data.Text (Text)

import qualified Data.Map.Strict as Map

atMay :: (Eq i, Num i) => [a] -> i -> Maybe a
atMay []       _ = Nothing
atMay (x : _)  0 = Just x
atMay (_ : xs) n = atMay xs $ n - 1

freshVars :: Fresh m => Text -> [b] -> m [(b, Name a)]
freshVars v = mapM (\ (i, b) -> (b, ) <$> freshVar (v <> showt i)) . zip [0 :: Int ..]

poly2Ty :: Fresh m => Poly -> m Ty
poly2Ty (Poly p) = snd <$> unbind p

projDefnTy :: Fresh m => Defn -> m Ty
projDefnTy Defn { defnPolyTy = Embed t } = poly2Ty t

-- TODO(chathhorn): what if we retype extrude and unfold (et al.?) with
-- concrete types and re-run type inference? Much of the muck here is just
-- calculating types that could be inferred.

-- | Transforms functions in state and resumption monads into plain first-order functions.
purify :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => Name Exp -> FreeProgram -> m FreeProgram
purify start (ts, syns, ds) = do
      (smds, notSmds)     <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods)         <- partitionEithers <$> mapM isReacMonadicDefn notSmds

      maybe (failAt noAnn $ "No definition for start function (" <> prettyPrint start <> ") found!")
            (projDefnTy >=> checkStartType)
            $ find (isStart . defnName) rmds

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      -- TODO(chathhorn): perhaps move to a type-checking phase?
      (i, o, ms, _) <- getCanonReacTy rmds >>= liftMaybe noAnn "Inconsistent ReacT types (i, o, and state layers must be the same for all ReacT)."

      rhoS      <- mkPureEnv ms nameNpolyS
      rhoR      <- mkPureEnv ms nameNpolyR
      let rho = rhoS <> rhoR

      pure_smds <- mapM (purifyStateDefn rho ms) smds

      iv <- freshVar "i"

      (pure_rmds, PSto points _ allAs) <- runStateT (mapM (purifyResDefn start rho ms) rmds) $ PSto mempty iv mempty

      let (dcs, pes) = unzip $ Map.elems points

      disp <- mkDispatch i o ms iv pes

      pure ( filter (not . isAorR) ts <> [mkRDatatype dcs] <> [mkADatatype $ Map.toList allAs]
           , syns
           , mkStart start i o ms : disp : ods <> pure_smds <> pure_rmds
           )

      where getCanonReacTy :: Fresh m => [Defn] -> m (Maybe (Ty, Ty, [Ty], Set Ty))
            getCanonReacTy = (getCanonReacTy' . catMaybes <$>) . mapM (projDefnTy >=> pure . dstReacT . codomTy)

            getCanonReacTy' :: [(Ty, Ty, [Ty], Ty)] -> Maybe (Ty, Ty, [Ty], Set Ty)
            getCanonReacTy' = \ case
                  []                  -> Nothing
                  (i, o, ms, a) : rs  -> foldr mergeReacTys (Just (i, o, ms, singleton a)) rs

            mergeReacTys :: (Ty, Ty, [Ty], Ty) -> Maybe (Ty, Ty, [Ty], Set Ty) -> Maybe (Ty, Ty, [Ty], Set Ty)
            mergeReacTys (i, o, ms, a) = \ case
                  Just (i', o', ms', a')
                        | length ms >= length ms'
                        , isSuffixOf ms' ms
                        , Just ui <- unify' i i', Just uo <- unify' o o'
                              -> Just (ui, uo, ms, insert a a')
                        | length ms < length ms'
                        , isSuffixOf ms ms'
                        , Just ui <- unify' i i', Just uo <- unify' o o'
                              -> Just (ui, uo, ms', insert a a')
                  _           -> Nothing

            isAorR :: DataDefn -> Bool
            isAorR = uncurry (||) . ((== "A_") &&& (== "R_")) . n2s . dataName

            isStart :: Name Exp -> Bool
            isStart = (== start)

            checkStartType :: MonadError AstError m => Ty -> m ()
            checkStartType t = case dstReacT t of
                  Just (_, _, [], _) -> pure ()
                  Just (_, _,  _, _) -> failAt (ann t) "Start definition must have type ReacT i o I (). Use the extrude function to remove state layers."
                  _                  -> failAt (ann t) "Start definition must have type ReacT i o I ()."

-- | In addition to all the above, we re-tie the recursive knot by adding a new
--   "start" as follows
--         start :: ReacT In Out I T
--         start = unfold dispatch start_pure
--           start :: In -> (Either T (O, R), ()) --- drop the "()"?

-- Correcting mkStart.
-- In the type of Main.start.
-- t (return type of Main.start) needs to be combined with ms (list of StateT s, stores).
mkStart :: Name Exp -> Ty -> Ty -> [Ty] -> Defn
mkStart start i o ms = Defn
      { defnAnnote = MsgAnnote "start function"
      , defnName   = start
      , defnPolyTy = [] |-> startTy
      , defnAttr   = Just NoInline
      , defnBody   = appl
      }
      where etor       = mkRangeTy o ms

            reacT i o a = TyCon (MsgAnnote "Purify: reacT") (s2n "ReacT") `tyApp` i `tyApp` o `tyApp` a
            startTy     = TyApp (MsgAnnote "Purify: startTy")
                              (reacT i o (TyCon (MsgAnnote "Purify: startTy") (s2n "Identity")))
                              nilTy

            unfold     = Builtin (MsgAnnote "Purify: unfold") Nothing (Just $ mkArrowTy [dispatchTy i o ms, etor] startTy) Unfold
            dispatch   = Var (MsgAnnote "Purify: dispatch") Nothing (Just $ dispatchTy i o ms) $ s2n "$Pure.dispatch"
            start_pure = Var (MsgAnnote "Purify: $Pure.start") Nothing (Just etor) $ s2n "$Pure.start"
            appl       = Embed $ bind [] $ mkApp (MsgAnnote "Purify: appl") unfold [dispatch, start_pure]

            tyApp :: Ty -> Ty -> Ty
            tyApp = TyApp $ MsgAnnote "reacT"

-- | Converts
-- >  (extrude (extrude (... (extrude dev s1) ...) s(n-1)) sn)
-- into
-- >  (dev, [s1, ..., sn])
-- Won't work if called on a non-extrude application.
flattenExtr :: Exp -> (Exp, [Exp])
flattenExtr = \ case
      App _ _ _ (App _ _ _ (Builtin _ _ _ Extrude) arg1) arg2 -> second (<> [arg2]) $ flattenExtr arg1
      e@(App _ _ _ _ rand)                                    -> first (const e) $ flattenExtr rand
      e                                                       -> (e, [])

-- | Generates the dispatch function.
-- > dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
mkDispatch :: (MonadError AstError m, Fresh m) => Ty -> Ty -> [Ty] -> Name Exp -> [(Pat, Exp)] -> m Defn
mkDispatch i o ms iv = \ case
      []        -> failAt NoAnnote "Purify: empty dispatch: invalid ReWire (is recursion guarded by signal?)"
      (p : pes) -> do
            disc     <- freshVar "disc"
            let ty    = dispatchTy i o ms
                domTy = tupleTy (MsgAnnote "Purify: mkDispatch: domTy") $ rTy : ms
                body  = Embed $ bind [disc :: Name Exp, iv :: Name Exp] cases
                cases = mkCases an (Var an Nothing (Just domTy) disc) p pes
                an    = MsgAnnote $ "Purify: generated dispatch function: " <> prettyPrint cases

            pure Defn
                  { defnAnnote = an
                  , defnName   = s2n "$Pure.dispatch"
                  , defnPolyTy = [] |-> ty
                  , defnAttr   = Just NoInline
                  , defnBody   = body
                  }

mkCases :: Annote -> Exp -> (Pat, Exp) -> [(Pat, Exp)] -> Exp
mkCases an disc = foldr mkCase' . flip (mkCase an disc) Nothing
      where mkCase' :: (Pat, Exp) -> Exp -> Exp
            mkCase' (p, e) = mkCase an disc (p, e) . Just

mkCase :: Annote -> Exp -> (Pat, Exp) -> Maybe Exp -> Exp
mkCase an disc (p, e) = Match an Nothing (typeOf e) disc (transPat p) $ mkLam' (p, e)
      where mkLam' :: (Pat, Exp) -> Exp
            mkLam' (p, e) = mkLam an (patVars p) e

mkRDatatype :: [DataCon] -> DataDefn
mkRDatatype dcs = DataDefn
       { dataAnnote = MsgAnnote "Purify: R Datatype"
       , dataName   = s2n "R_"
       , dataKind   = KStar
       , dataCons   = dcs
       }

mkRDataCon :: Annote -> Name DataConId -> [Ty] -> DataCon
mkRDataCon an r_g ts = DataCon an r_g $ [] |-> mkArrowTy ts rTy

mkADatatype :: [(Ty, Name DataConId)] -> DataDefn
mkADatatype allAs = DataDefn
       { dataAnnote = MsgAnnote "Purify: A Datatype"
       , dataName   = s2n "A_"
       , dataKind   = KStar
       , dataCons   = map mkADataCon allAs
       }

mkADataCon :: (Ty, Name DataConId) -> DataCon
mkADataCon (t, n) = DataCon (MsgAnnote "Purify: generated A data ctor") n $ [] |-> mkArrowTy [t] aTy

type PureEnv = [(Name Exp, Ty)]

mkPureEnv :: (Fresh m, MonadError AstError m) => [Ty] -> [(Name Exp, Embed Poly)] -> m PureEnv
mkPureEnv _ []                     = pure []
mkPureEnv ms ((n, Embed phi) : nps) = do
      ty     <- poly2Ty phi
      purety <- purifyTy (ann ty) ms $ Just ty
      ((n, purety) :) <$> mkPureEnv ms nps

getStates :: Ty -> [Ty]
getStates t = case dstReacT $ codomTy t of
      Just (_, _, sts', _) -> sts'
      _                    -> fromMaybe [] $ dstTyApp (codomTy t) >>= dstStateT . fst

lookupPure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupPure an x = maybe (failAt an $ "No pure binding for variable: " <> n2s x) pure . lookup x

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isStateT <$> poly2Ty poly

isReacMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isReacMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isReacT <$> poly2Ty poly

purifyStateDefn :: (Fresh m, MonadError AstError m, MonadIO m) =>
                   PureEnv -> [Ty] -> Defn -> m Defn
purifyStateDefn rho ms d = do
      ty           <- poly2Ty phi
      let ms'       = getStates ty
      p_pure       <- lookupPure (ann d) (defnName d) rho
      let d_pure    = defnName d
      (args, e)    <- unbind body
      nstos        <- freshVars "sigma" ms
      let stos      = toVar (ann d) <$> nstos
      e'           <- purifyStateBody rho stos ms (length ms - length ms') e
      let b_pure    = bind (args <> (snd <$> nstos)) e'
      pure $ d { defnName = d_pure, defnPolyTy = [] |-> p_pure, defnBody = Embed b_pure }
      where Embed body = defnBody d
            Embed phi  = defnPolyTy d

liftMaybe :: MonadError AstError m => Annote -> Text -> Maybe a -> m a
liftMaybe an msg = maybe (failAt an msg) pure

purifyResDefn :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => Name Exp -> PureEnv -> [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn start rho ms d = do
      ty            <- projDefnTy d
      (i, o, _, a)  <- liftMaybe (ann d) "Purify: failed at purifyResDefn" $ dstReacT $ codomTy ty
      pure_ty       <- purifyTy (ann d) ms $ Just ty
      (args, e)     <- unbind body

      (nstos, stos) <- (map snd &&& map (toVar an)) <$> freshVars "sto" ms
      e'            <- purifyResBody start rho i o a stos ms e

      --
      -- Below is an egregious hack to compile the start symbol slightly differently.
      --
      let args'  = if isStart then [] else args
          nstos' = if isStart then [] else nstos
          p_pure = if isStart then [] |-> codomTy pure_ty else [] |-> pure_ty
          d_pure = if isStart then s2n "$Pure.start" else dname
          b_pure = bind (args' <> nstos') e'

      pure $ d { defnName = d_pure, defnPolyTy = p_pure, defnBody = Embed b_pure }
      where dname      = defnName d
            Embed body = defnBody d
            an         = defnAnnote d

            isStart :: Bool
            isStart = dname == start

---------------------------
-- Purifying Types
---------------------------

data TyVariety = Arrow !Ty !Ty | ReacTApp | StateTApp | IdApp | PairApp !Ty !Ty | Pure

purifyTy :: MonadError AstError m => Annote -> [Ty] -> Maybe Ty -> m Ty
purifyTy an _ Nothing   = failAt an "Purify: purifyTy: encountered untyped expression (rwc bug)."
purifyTy an ms (Just t) = case classifyTy t of
      Arrow t1 t2   -> TyApp an <$> (TyApp an (TyCon an $ s2n "->") <$> purifyTy an ms (Just t1)) <*> purifyTy an ms (Just t2)
      ReacTApp      -> liftMaybe an ( "Purify: failed to purify ResT type: " <> prettyPrint t)
                                    $ purifyResTy t
      StateTApp     -> liftMaybe an ( "Purify: failed to purify StateT type: " <> prettyPrint t)
                                    $ purifyStateTTy ms t
      PairApp t1 t2 -> TyApp an <$> (TyApp an (TyCon an $ s2n "(,)") <$> purifyTy an ms (Just t1)) <*> purifyTy an ms (Just t2)
      Pure          -> pure t
      IdApp         -> failAt an "Purify: purifyTy: encountered Identity."

            -- This takes a Ty of the form
            -- >    T1 -> T2 -> ... -> Tn -> ReacT In Out (StateT S1 (StateT S2 (... (StateT Sm I)))) T
            -- and returns a Ty of the form
            -- >    T1 -> T2 -> ... -> Tn -> In -> S1 -> ... -> Sm -> (Either T (O, R), (S1, (..., Sm)))
      where purifyResTy :: Ty -> Maybe Ty
            purifyResTy t = do
                  (_, o, _, _) <- dstReacT $ codomTy t
                  pure $ mkArrowTy (paramTys t <> ms) $ mkRangeTy o ms

            -- Takes
            -- >    T1 -> T2 -> ... -> Tn -> StateT S1 (StateT S2 (... (StateT Sm I))) T
            -- and replaces it by
            -- >    T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T, (S1, (..., Sm)))
            --
            -- I'm going to rewrite this to stick precisely to Adam's description.
            -- N.b., the product in the codomain associates to the right. The
            -- definition of purifyStateTTy required a little hackery to get the result
            -- into that form.
            purifyStateTTy :: [Ty] -> Ty -> Maybe Ty
            purifyStateTTy ms t = do
                  a <- snd <$> dstTyApp (codomTy t)
                  pure $ mkArrowTy (paramTys t <> ms) $ tupleTy (MsgAnnote "Purify: purifyStateTTy") $ a : ms

            classifyTy :: Ty -> TyVariety
            classifyTy = \ case
                  t | isReacT t                                                  -> ReacTApp
                  TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2                -> Arrow t1 t2
                  TyApp _ (TyApp _ (TyCon _ (n2s -> "(,)")) t1) t2               -> PairApp t1 t2
                  TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "StateT")) _) _) _  -> StateTApp
                  TyApp _ (TyCon _ (n2s -> "Identity")) _                        -> IdApp
                  _                                                              -> Pure

---------------------------
-- Purifying State Monadic definitions
---------------------------

classifyCases :: (Fresh m, MonadError AstError m) => Exp -> m Cases
classifyCases ex = case flattenApp ex of
      (Builtin an _ t Get       , [])     -> pure $ CGet an t
      (Builtin an _ t Return    , [e])    -> pure $ CReturn an t e
      (Builtin an _ _ Put       , [e])    -> pure $ CPut an e
      (Builtin an _ _ Lift      , [e])    -> pure $ CLift an e
      (Builtin an _ _ Bind      , [e, g]) -> pure $ CBind an e g
      (Match an _ _ disc p e els, [])     -> pure $ CMatch an disc p e els
      (Builtin an _ _ Error     , _)      -> failAt an "Purify: encountered unsynthesizable definition."
      (Var an _ t g             , es)     -> pure $ CApply an t g es
      d                                   -> failAt (ann ex) $ "Purify: unclassifiable case: " <> prettyPrint d

data Cases = CGet Annote !(Maybe Ty)
           | CReturn Annote !(Maybe Ty) !Exp
           | CLift Annote !Exp
           | CPut Annote !Exp
           | CBind Annote !Exp !Exp
           | CApply Annote !(Maybe Ty) !(Name Exp) ![Exp]
           | CMatch Annote !Exp !MatchPat !Exp !(Maybe Exp)

-- | purifyStateBody
--   rho  -- pure environment
--   stos -- state vars
--   stys -- state types
--   i    -- lifting "depth"
--   tm   -- term to be purified
purifyStateBody :: (Fresh m, MonadError AstError m, MonadIO m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> Exp -> m Exp
purifyStateBody rho stos stys i = classifyCases >=> \ case
      CGet an _        -> do
            s <- liftMaybe an ("Purify: state-layer mismatch: length stos == " <> showt (length stos) <> ", i == " <> showt i)
                  $ stos `atMay` i
            pure $ mkTuple an $ s : stos

      CReturn an _ e   -> pure $ mkTuple an $ e : stos

      CLift _ e        -> purifyStateBody rho stos stys (i + 1) e

      CPut an e        -> mkTuple an . (nil :) <$> replaceAtIndex an i e stos

      CApply an _ n es -> mkPureApp an rho n $ es <> stos

      -- e1 must be simply-typed, so don't purify it.
      CMatch an e1 mp e2 e3 -> do
            p   <- transMPat mp
            e2' <- purifyStateBody rho stos stys i $ mkApp an e2 $ toVar an <$> patVars p
            mkCase an e1 (p, e2') <$> mapM (purifyStateBody rho stos stys i) e3

      CBind an e g -> do
            a           <- liftMaybe (ann e) "Purify: invalid type in bind" $ typeOf e >>= (fmap snd . dstTyApp)
            ns          <- freshVars "st" $ a : stys
            (f, es)     <- dstApp g
            g_pure_app  <- mkPureApp an rho f $ es <> (toVar an <$> ns)
            e'          <- purifyStateBody rho stos stys i e
            let p        = mkTuplePat an $ patVar <$> ns
            mkLet an p e' g_pure_app

      where replaceAtIndex :: MonadError AstError m => Annote -> Int -> a -> [a] -> m [a]
            replaceAtIndex an n item ls = case splitAt n ls of
                  (a, _ : b) | n >= 0 -> pure $ a <> (item : b)
                  _                   -> failAt an "Purify: replaceAtIndex: invalid index (this should never happen)"

---------------------------
-- Purifying Resumption Monadic definitions
---------------------------

-- It may be that the type for each subexpression occurring in here should be
-- part of the annotation here. That is partially accomplished right now.
data RCase = RReturn Annote !Exp
           | RLift Annote !Exp
           | RVar Annote !(Maybe Ty) !(Name Exp)
           | RSignal Annote !Exp
           | RBind Annote !Exp !Exp
           | RSigK Annote !Ty !Exp !(Name Exp) ![(Exp, Ty)] -- (signal e >>= g e1 ... ek)
           | RExtrude Annote !(Maybe Ty) ![Exp]                     -- [(e, t)]
           | RApp Annote !(Maybe Ty) !(Name Exp) ![Exp]
           | RMatch Annote !Exp !MatchPat !Exp !(Maybe Exp)

instance TextShow RCase where
      showb = \ case
            RReturn  {} -> fromText "RReturn"
            RLift    {} -> fromText "RLift"
            RVar     {} -> fromText "RVar"
            RSignal  {} -> fromText "RSignal"
            RBind    {} -> fromText "RBind"
            RSigK    {} -> fromText "RSigK"
            RExtrude {} -> fromText "RExtrude"
            RApp     {} -> fromText "RApp"
            RMatch   {} -> fromText "RMatch"

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m RCase
classifyRCases ex = case flattenApp ex of
      (Builtin an _ _ Bind       , [sig -> Just s, flattenApp -> (Var _ _ (Just t) g', es)])
                                           -> pure $ RSigK an t s g' $ zip es $ paramTys t
      (Builtin an _ _ Bind       , [e, g]) -> pure $ RBind an e g
      (Builtin an _ _ Return     , [e])    -> pure $ RReturn an e
      (Builtin an _ _ Lift       , [e])    -> pure $ RLift an e
      (Builtin an _ _ Signal     , [e])    -> pure $ RSignal an e
      (Builtin an _ t Extrude    , es)     -> pure $ RExtrude an t es
      (Builtin an _ _ Error      , _)      -> failAt an "Purify: encountered unsynthesizable definition."
      (Match an _ _ disc p e els , [])     -> pure $ RMatch an disc p e els
      (Var an _ t x              , [])     -> pure $ RVar an t x
      (Var an _ t x              , es)     -> pure $ RApp an t x es
      d                                  -> failAt (ann ex) $ "Purify: unclassifiable R-case: " <> prettyPrint d
      where sig :: Exp -> Maybe Exp
            sig ex = case flattenApp ex of
                  (Builtin _ _ _ Signal, [arg]) -> pure arg
                  _                             -> Nothing

-- state for res-purification.
data PSto = PSto !(Map (Name Exp) ResPoint) !(Name Exp) !(Map Ty (Name DataConId))
type ResPoint = (DataCon, (Pat, Exp))

-- | purifyResBody
--  rho    -- pure environment
--  i      -- input type
--  o      -- output type
--  t      -- return type of monad (I'm assuming it's pure)
--  stys   -- state types [S1, ..., Sm]
--  stos   -- input states [s1, ..., sm]
--  tm     -- term to be purified
purifyResBody :: (Fresh m, MonadError AstError m, MonadIO m)
              => Name Exp -> PureEnv -> Ty -> Ty -> Ty -> [Exp] -> [Ty] -> Exp -> StateT PSto m Exp
purifyResBody start rho i o a stos ms = classifyRCases >=> \ case
      -- purifyResBody (return e)         = "(Left (e, (s1, (..., sm))))"
      RReturn _ e | Just t <- typeOf e -> mkLeft "RReturn" e t stos
      RReturn an _                     -> failAt an "Purify: purifyResBody: untyped expression encountered (rwc bug)."

      -- purifyResBody (signal e
      --        >>= g e1 ... ek) = "Right ((e, (s1, (..., sm))), R_g e1 ... ek)"
      --           Side effect:
      --     * add the clause "R_g T1 ... Tk" to R
      --     (where Ti is the type of ei)
      --     * add the eqn
      --         dispatch ((R_g e1 ... ek), (s1, ..., sm)) i
      --      = g_pure e1 ... ek i s1 ... sm
      -- to defn of dispatch
      RSigK an _ e g bes -> do
            let (es, ts) = unzip bes
                r_g      = s2n $ "R_" <> prettyPrint g

            ts' <- mapM (purifyTy an ms . Just) ts

            (p, xs) <- mkRPat an ts' r_g           -- Pattern (R_g e1 ... ek)
            ns      <- freshVars "s" ms
            let pairpat = mkTuplePat an $ p : (patVar <$> ns) -- Pattern (R_g e1 ... ek, (s1, ..., sn))

            let svars = uncurry (Var an Nothing . Just) <$> ns  -- [s1, ..., sn]
            let vars  = zipWith (flip $ Var an Nothing . Just) xs ts'   -- [x1, ..., xn]
            iv <- getI
            g_pure_app <- mkPureApp an rho g $ vars <> (Var an Nothing (Just i) iv : svars)
            -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
            addResPoint g (mkRDataCon an r_g ts') (pairpat, g_pure_app) -- "R_g T1 ... Tk"

            let rGes  = mkApp an (Con an Nothing (Just $ mkArrowTy ts' rTy) r_g) es
            let outer = mkTuple an $ [e, rGes] <> stos
            pure $ mkRight outer             -- Right ((e, R_g e1 ... ek), (s1, (..., sm)))

      -- purifyResBody (signal e)         = "(Right ((e, (s1, (..., sm))), R_ret))"
      RSignal an e -> do
            addNakedSignal an
            pure $ mkRight $ mkTuple an $ [e, Con an Nothing (Just rTy) $ s2n "R_return"] <> stos

      -- purifyResBody (e >>= g)          = "let
      --   -- N.B.: The irrefutable pattern here is
      --   -- sketchy, but it should be okay because
      --   -- of the restriction on occurrences of
      --   -- "signal"
      --               Left (v, (s1, (..., sm))) = [|purifyResBody e|]
      --             in
      --               g_pure v s1 ... sm"
      -- It appears to me that g can be an application (and not only a variable). In which case, I guess
      -- the right thing to do is just apply "v s1 ... sm" to g. And possibly add "_pure" to the function at the
      -- head of the application. The way it's written below assumes that g is a variable.
      RBind an e g -> do
            -- purify the types of e and g
            tg'         <- purifyTy an ms $ typeOf g
            -- ert is the return type of e; ty is the type of the whole expression
            (ert, _)    <- maybe (failAt (ann tg') "Purify: expecting arrow, encountered non-arrow") pure
                              $ dstArrow tg'
            e'          <- purifyResBody start rho i o ert stos ms e

            -- start calculating pattern: p = (Left (v, (s1, (..., sm))))
            svars       <- freshVars "s" ms
            v           <- freshVar "v"
            p           <- mkLeftPat "RBind" (patVar (ert, v)) ert (patVar <$> svars)
            -- done calculating p = Left (v, (s1, (..., sm)))

            -- calculating g_pure_app = "g_pure v s1 ... sm"
            let vars     = toVar an <$> svars
            (f, es)     <- dstApp g
            g_pure_app  <- mkPureApp an rho f $ es <> (Var an Nothing (Just ert) v : vars)
            -- done calculating g_pure_app
            mkLet an p e' g_pure_app

          -- N.b., the pure env rho will have to contain *all* purified bindings
          --       or this will fail.
      RLift an e -> do
            e'    <- purifyStateBody rho stos ms 0 e
            s_i   <- freshVars "s" ms
            v     <- freshVar "v"
            -- the pattern "(v, (s1, (..., sm)))"
            let p  = mkTuplePat an $ patVar <$> (a, v) : s_i
            body  <- mkLeft "RLift" (Var an Nothing (Just a) v) a $ toVar an <$> s_i

            mkLet an p e' body

      RVar an tx x   -> do
            tx' <- purifyTy an ms tx
            pure $ mkApp an (Var an Nothing (Just tx') x') stos
                  where x' = if isStart x then s2n "$Pure.start" else x

      -- extrude :: Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m a
      --
      -- To purify e = (extrude ... (extrude phi s1) ... sn):
      -- 1. N.b., phi :: ReacT i o (StateT s m) a. We'll assume that e is "maximal". I.e.,
      --    that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
      -- 2. phi' <- purifyResBody phi
      -- 3. make definition and add it to definitions: $LL = phi'
      -- 4. return $ (...($LL s1)...sn)
      RExtrude an t rands -> case flattenExtr $ mkApp an (Builtin an Nothing t Extrude) rands of
            (Var an tan t d, stos) -> do
                  t'  <- purifyTy an ms t
                  pure $ mkApp an (Var an tan (Just t') d) stos
            (e@App {}, stos)   -> do
                  (f, es)    <- dstApp e
                  t'         <- purifyTy (ann e) ms t
                  pure $ mkApp (ann e) (Var (ann e) Nothing (Just t') f) $ es <> stos
            (e, _)             -> failAt (ann e) $ "Purify: extruded device is non-variable: " <> prettyPrint e

      RApp an rty rator rands -> do
            rator' <- purifyResBody start rho i o a stos ms (Var an Nothing rty rator)
            -- N.b., don't think it's necessary to purify the rands because
            -- they're simply typed.
            (f, stos') <- dstApp rator'
            mkPureApp an rho f $ rands <> stos'

       -- disc must be simply-typed, so don't purify it.
      RMatch an disc mp e els -> do
            p  <- transMPat mp
            e' <- purifyResBody start rho i o a stos ms $ mkApp an e $ toVar an <$> patVars p
            mkCase an disc (p, e') <$> mapM (purifyResBody start rho i o a stos ms) els

      where mkLeft :: (Fresh m, MonadError AstError m, MonadState PSto m) => Text -> Exp -> Ty -> [Exp] -> m Exp
            mkLeft s a t stos = do
                  let an = ann a
                  c <- getACtor s t
                  let anode = mkApp an (Con an Nothing (Just $ mkArrowTy [t] aTy) c) [a]
                  pure $ mkApp an (Con an Nothing (Just $ mkArrowTy [tupleTy an $ aTy : ms] $ mkRangeTy o ms) (s2n "Done")) [mkTuple an $ anode : stos]

            mkLeftPat :: (Fresh m, MonadError AstError m) => Text -> Pat -> Ty -> [Pat] -> StateT PSto m Pat
            mkLeftPat s a t stos = do
                  let an = ann a
                  c <- getACtor s t
                  let anode =  PatCon an (Embed Nothing) (Embed $ Just aTy) (Embed c) [a]
                  pure $ PatCon an (Embed Nothing) (Embed $ Just $ mkRangeTy o ms) (Embed $ s2n "Done") [mkTuplePat an $ anode : stos]

            mkRight :: Exp -> Exp
            mkRight e = mkApp (ann e) (Con (ann e) Nothing (mkArrowTy <$> (pure <$> typeOf e) <*> pure (mkRangeTy o ms)) (s2n "Pause")) [e]

            addNakedSignal :: (Fresh m, MonadError AstError m, MonadState PSto m) => Annote -> m ()
            addNakedSignal an = do
                  -- by default, must add the eqn: dispatch R_return i = Left i
                  let r_return = s2n "R_return"

                  p_ret     <- fst <$> mkRPat an [] r_return
                  p_ret_ns  <- freshVars "s" ms
                  let p_ret' = mkTuplePat an $ p_ret : (patVar <$> p_ret_ns)

                  iv        <- getI
                  b_ret'    <- mkLeft "Signal" (Var an Nothing (Just i) iv) i $ uncurry (Var an Nothing . Just) <$> p_ret_ns
                  addResPoint r_return (mkRDataCon an r_return []) (p_ret', b_ret')

            getI :: MonadState PSto m => m (Name Exp)
            getI = get >>= (\ (PSto _ iv _) -> pure iv)

            addResPoint :: MonadState PSto m => Name Exp -> DataCon -> (Pat, Exp) -> m ()
            addResPoint gid dc ps = modify (\ (PSto pts iv as) -> PSto (Map.insert gid (dc, ps) pts) iv as)

            getACtor :: (Fresh m, MonadState PSto m) => Text -> Ty -> m (Name DataConId)
            getACtor s t = do
                  PSto _ _ as <- get
                  case Map.lookup (unAnn t) as of
                        Nothing -> do
                              c <- freshVar $ "A_" <> s <> showt (Map.size as)
                              -- TODO(chathhorn): the num postfixed by Fresh is dropped in toCore because ctors assumed unique.
                              modify (\ (PSto pes iv as) -> PSto pes iv $ Map.insert (unAnn t) c as)
                              pure c
                        Just c -> pure c

            isStart :: Name Exp -> Bool
            isStart = (== start)

dispatchTy :: Ty -> Ty -> [Ty] -> Ty
dispatchTy i o ms = mkArrowTy [domTy, i] etor
    where etor  = mkRangeTy o ms
          domTy = tupleTy (MsgAnnote "Purify: dispatchTy") $ rTy : ms

-- | Global constant representation of R data type.
rTy :: Ty
rTy = TyCon (MsgAnnote "Purify: rTy") (s2n "R_")

aTy :: Ty
aTy = TyCon (MsgAnnote "Purify: aTy") (s2n "A_")

patVar :: (Ty, Name Exp) -> Pat
patVar = toPatVar (MsgAnnote "Purify: patVar")

mkRangeTy :: Ty -> [Ty] -> Ty
mkRangeTy o ts = mkEitherTy (tupleTy (MsgAnnote "Purify: mkRangeTy") ts) o

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp (MsgAnnote "Purify: mkEitherTy") (TyApp (MsgAnnote "Purify: mkEitherTy") (TyCon (MsgAnnote "Purify: mkEitherTy") $ s2n "PuRe") t1)

mkRPat :: Fresh m => Annote -> [Ty] -> Name DataConId -> m (Pat, [Name Exp])
mkRPat an ts r_g = do
      xs <- freshVars "store" ts
      let varpats = toPatVar an <$> xs
      pure (PatCon an (Embed Nothing) (Embed $ Just $ TyCon an $ s2n "R_") (Embed r_g) varpats, snd <$> xs)

mkPureVar :: MonadError AstError m => Annote -> PureEnv -> Name Exp -> m Exp
mkPureVar an rho x = Var an Nothing <$> (Just <$> lookupPure an x rho) <*> pure x

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Name Exp -> [Exp] -> m Exp
mkPureApp an rho rator es = flip (mkApp an) es <$> mkPureVar an rho rator

dstApp :: MonadError AstError m => Exp -> m (Name Exp, [Exp])
dstApp e = case flattenApp e of
      (Var _ _ _ n, es) -> pure (n, es)
      _                 -> failAt (ann e) $ "Purify: tried to dst non-app: " <> showt (unAnn e)

-- | Lets are desugared already, so use a case instead (with lifted discriminator).
mkLet :: Fresh m => Annote -> Pat -> Exp -> Exp -> m Exp
mkLet an p e1 e2 = do
      v <- freshVar "disc"
      -- Need to lift the discriminator.
      pure $ mkApp an (Lam an Nothing (arrowLeft <$> typeOf e1) $ bind v $ mkCase an (Var an Nothing (typeOf e1) v) (p, e2) Nothing) [e1]
