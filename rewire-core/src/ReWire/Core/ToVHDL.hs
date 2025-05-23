{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.ToVHDL (compileProgram) where

import ReWire.Config (Config, vhdlPackages, reset, resetFlags, ResetFlag (..))
import ReWire.Annotation (Annote, Annotated (ann), noAnn)
import ReWire.Core.Syntax as C hiding (Name, Index, Size)
import ReWire.Error (failAt, MonadError, AstError)
import ReWire.Core.Mangle (mangle)
import ReWire.VHDL.Syntax as V
import ReWire.BitVector (width, nat)
import ReWire.Pretty (showt)

import Control.Lens ((^.))
import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (StateT (..), get, put, lift)
import Data.Bits (testBit)
import Data.List (genericLength, find)
import Data.Text (Text)

type CM m = StateT ([Signal], [Component], Index) (ReaderT [Defn] m)

askDefns :: Monad m => CM m [Defn]
askDefns = ask

nvec :: Value -> Size -> [Bit]
nvec n width = nvec' 0 []
      where nvec' :: Index -> [Bit] -> [Bit]
            nvec' pos bits | pos >= fromIntegral width = bits
                           | otherwise                 = nvec' (pos + 1) $ (if testBit n $ fromEnum pos then One else Zero) : bits

getTyPorts :: Monad m => C.Sig -> CM m [Port]
getTyPorts (Sig _ argsizes ressize) = do
      let argnames = zipWith (\ _ x -> "arg" <> showt x) argsizes [0::Int ..]
          argports = zipWith (\ n x -> Port n In (TyStdLogicVector x)) argnames argsizes
          resport  = Port "res" Out (TyStdLogicVector ressize)
      pure $ argports <> [resport]

mkDefnEntity :: Monad m => Defn -> CM m Entity
mkDefnEntity (Defn _ n t _) = Entity (mangle n) <$> getTyPorts t

freshName :: Monad m => Text -> CM m Name
freshName s = do
      (sigs, comps, ctr) <- get
      put (sigs, comps, ctr + 1)
      pure (s <> showt ctr)

addSignal :: Monad m => Text -> V.Ty -> CM m ()
addSignal n t = do
      (sigs, comps, ctr) <- get
      put (sigs <> [Signal n t], comps, ctr)

addComponent :: Monad m => Annote -> Name -> C.Sig -> CM m ()
addComponent _ i t = do
      (sigs, comps, ctr) <- get
      case find ((== mangle i) . componentName) comps of
            Just _ -> pure ()
            Nothing -> do
                  ps <- getTyPorts t
                  put (sigs, Component (mangle i) ps : comps, ctr)

-- | First Expr is for whether match (std_logic); remaining Exprs are for extracted fields.
compilePat :: Monad m => Name -> Index -> Pat -> CM m (Expr, [Expr])
compilePat nscr offset = \ case
      PatVar _ s       -> pure (ExprBoolConst True, [ExprSlice (ExprName nscr) offset (offset + fromIntegral s - 1)])
      PatWildCard _ _  -> pure (ExprBoolConst True, [])
      PatLit _ bv      -> do
            let tagSize  = fromIntegral $ width bv
                tagValue = if tagSize > 0 then nat bv else 0
                tag      = nvec tagValue tagSize
                ematch   = ExprIsEq (ExprSlice (ExprName nscr) offset (offset + genericLength tag - 1)) $ ExprBitString tag
            pure (ematch, [])

askGIdTy :: MonadError AstError m => Name -> CM m C.Sig
askGIdTy i = do
      defns <- askDefns
      case find (\ (Defn _ i' _ _) -> i == i') defns of
            Just (Defn _ _ t _) -> pure t
            Nothing             -> lift $ failAt noAnn $ "askGIdTy: no info for identifier " <> showt i

compileExps :: MonadError AstError m => [C.Exp] -> CM m ([Stmt], Name)
compileExps es = do
      n          <- (<> "Res") <$> freshName (mangle "slice")
      addSignal n $ TyStdLogicVector $ sum $ map sizeOf es
      sssns      <- mapM compileExp es
      let stmts   = concatMap fst sssns
          ns      = map snd sssns
      pure  ( stmts <>
                  [ Assign (LHSName n)
                        $ simplifyConcat
                        $ foldl' ExprConcat (ExprBitString [])
                        $ map ExprName ns
                  ]
            , n
            )

compileExp :: MonadError AstError m => C.Exp -> CM m ([Stmt], Name)
compileExp = \ case
      LVar _ _ i       -> pure ([], "arg" <> showt i)
      Concat _ e1 e2   -> compileExps $ gather e1 <> gather e2
      Lit _ bv         -> do
            let litSize  = fromIntegral $ width bv
                litValue = if litSize > 0 then nat bv else 0
            n          <- (<> "Res") <$> freshName (mangle $ "lit" <> showt litValue <> "x" <> showt litSize)
            addSignal n $ TyStdLogicVector litSize
            let litVec  = nvec litValue litSize
            pure  ([ Assign (LHSName n) $ ExprBitString litVec ], n)
      Call an sz (Global gid) escr ps ealt | isNil ealt -> do
            (stmts_escr, n_escr) <- compileExp escr

            let fieldWidths       = map sizeOf ps
                fieldOffsets      = init $ scanl (+) 0 fieldWidths
            efields              <- concatMap snd <$> zipWithM (compilePat n_escr . fromIntegral) fieldOffsets ps

            n_gid                <- (<> "Res") <$> freshName (mangle gid)
            addSignal n_gid $ TyStdLogicVector sz
            n_call               <- (<> "Call") <$> freshName (mangle gid)
            t_gid                <- askGIdTy gid
            addComponent an gid t_gid
            let argns            =  map (("arg" <>) . showt) [0::Index ..]
                pm               =  PortMap (zip argns efields <> [("res", ExprName n_gid)])
            pure (stmts_escr <> [Instantiate n_call (mangle gid) pm], n_gid)
      Call an sz (Global gid) escr ps ealt -> do
            n                    <- (<> "Res") <$> freshName "match"
            addSignal n $ TyStdLogicVector sz
            (stmts_escr, n_escr) <- compileExp escr

            let fieldWidths       = map sizeOf ps
                fieldOffsets      = init $ scanl (+) 0 fieldWidths
            rs                   <- zipWithM (compilePat n_escr . fromIntegral) fieldOffsets ps
            let ematch            = foldl' ExprAnd (ExprBoolConst True) $ map fst rs
                efields           = concatMap snd rs

            n_gid                <- (<> "Res") <$> freshName (mangle gid)
            addSignal n_gid $ TyStdLogicVector sz
            n_call               <- (<> "Call") <$> freshName (mangle gid)
            (stmts_ealt, n_ealt) <- compileExp ealt
            t_gid                <- askGIdTy gid
            addComponent an gid t_gid
            let argns             = map (("arg" <>) . showt) [0::Index ..]
                pm                = PortMap (zip argns efields <> [("res", ExprName n_gid)])
            pure (stmts_escr <>
                    [WithAssign ematch (LHSName n)
                                [(ExprName n_gid, ExprBoolConst True)]
                                 (Just (ExprName n_ealt)),
                     Instantiate n_call (mangle gid) pm] <>
                    stmts_ealt,
                    n)
      e -> failAt (ann e) "ToVHDL: compileExp: encountered currently unsupported expression."
      -- TODO(chathhorn): extern
      -- Extern _ sz i args -> do
      --       n           <- (<> "Res") <$> freshName i
      --       addSignal n $ TyStdLogicVector sz
      --       sssns       <- mapM compileExp args
      --       let stmts   =  concatMap fst sssns
      --           ns      =  map snd sssns
      --       pure (stmts <> [Assign (LHSName n) (ExprFunCall i (map ExprName ns))], n)

mkDefnArch :: MonadError AstError m => Defn -> CM m Architecture
mkDefnArch (Defn _ n _ body) = do
      put ([], [], 0) -- empty out the signal and component store, reset name counter
      (stmts, nres)    <- compileExp body
      (sigs, comps, _) <- get
      pure $ Architecture (mangle n <> "Impl") (mangle n) sigs comps (stmts <> [Assign (LHSName "res") (ExprName nres)])

-- TODO(chathhorn): support breaking out states (sts).
compileStart :: MonadError AstError m => Config -> Name -> C.Wiring -> C.GId -> C.Sig -> C.GId -> C.Sig -> CM m Unit
compileStart conf topLevel w n_loopfun sigLoop n_startstate sigState0 = do
      put ([], [], 0) -- empty out signal and component store, reset name counter
      let stateSize = sizeOf sigState0
          inpSize   = sum $ map snd inps
          outpSize  = sum $ map snd outps
      addComponent noAnn n_startstate sigState0
      addComponent noAnn n_loopfun sigLoop
      addSignal "start_state"        $ TyStdLogicVector stateSize
      addSignal "loop_out"           $ TyStdLogicVector stateSize
      addSignal "current_state"      $ TyRegister "clk" stateSize
      addSignal "done_or_next_state" $ TyStdLogicVector stateSize
      addSignal "next_state"         $ TyStdLogicVector stateSize
      addSignal "inp"                $ TyStdLogicVector inpSize
      let ports =
            [ Port "clk"  In TyClock
            , Port rst    In TyStdLogic
            ] <> zipWith (\ n s -> Port n In  (TyStdLogicVector s)) (map fst inps)  (map snd inps)
              <> zipWith (\ n s -> Port n Out (TyStdLogicVector s)) (map fst outps) (map snd outps)
      (sigs, comps, _) <- get
      pure $ Unit (conf^.vhdlPackages) (Entity topLevel ports) $
            Architecture (topLevel <> "_impl") topLevel sigs comps
                  [ Instantiate "start_call" (mangle n_startstate) (PortMap [("res", ExprName "start_state")])
                  , Instantiate "loop_call" (mangle n_loopfun) (PortMap
                        [ ("arg0", ExprSlice (ExprName "current_state") (1 + fromIntegral outpSize) (1 + fromIntegral (outpSize + arg0size) - 1))
                        , ("arg1", ExprName "inp")
                        , ("res", ExprName "loop_out")
                        ] )
                  , WithAssign (ExprName rst) (LHSName "next_state")
                        [ (ExprName "start_state", ExprBit rstSignal) ]
                        (Just (ExprName "done_or_next_state"))
                  , WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "done_or_next_state")
                        [ (ExprName "loop_out", ExprBitString [One]) ]
                        (Just (ExprName "current_state"))
                  , ClkProcess "clk" $ Assign (LHSName "current_state") (ExprName "next_state") : outputs
                  , Assign (LHSName "inp") $ foldl' ExprConcat (ExprBitString []) $ map (ExprName . fst) inps

             -- TODO(chathhorn): fix output.
             --      , WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "outp")
             --               [ ( ExprConcat (ExprBitString [One])
             --                        ( ExprConcat (ExprSlice (ExprName "current_state") 1 outsize)
             --                              pad_for_out ) -- "1" & (current_state[1 to outsize] & pad_for_out)
             --                                            -- MSB
             --                 , ExprBitString [One]
             --                 )
             --               ]
             --           (Just ( ExprConcat (ExprBitString [Zero])
             --                        ( ExprConcat (ExprSlice (ExprName "current_state") 1 ressize)
             --                              pad_for_res) ) --  "0" & (current_state[1 to ressize] & pad_for_res)
             --           )
                  ]
      where rstSignal :: Bit
            rstSignal | Inverted `elem` (conf^.resetFlags) = Zero
                      | otherwise                          = One

            rst :: Text
            rst = conf^.reset

            outputs :: [Stmt]
            outputs = fst $ foldl' (\ (as, off) (n, sz) ->
                  (as <> [Assign (LHSName n) (ExprSlice (ExprName "current_state") off (off + fromIntegral sz - 1))], off + fromIntegral sz))
                  ([], 1) outps

            inps = inputWires w
            outps = outputWires w
            arg0size = case sigLoop of
                  Sig _ (a : _) _ -> a
                  _               -> error "ToVHDL: arg0size: empty arg0 (rwc bug)."

compileDefn :: MonadError AstError m => Config -> Defn -> CM m Unit
compileDefn conf d = Unit (conf^.vhdlPackages) <$> mkDefnEntity d <*> mkDefnArch d

compileProgram :: MonadError AstError m => Config -> C.Device -> m V.Device
compileProgram conf p = fmap fst $ flip runReaderT (defns p) $ flip runStateT ([], [], 0)
      $ V.Device
            <$> ((:)
            <$> compileStart
                  conf
                  (C.topLevel p)
                  (C.wiring p)
                  (defnName $ C.loop p)
                  (defnSig $ C.loop p)
                  (defnName $ C.state0 p)
                  (defnSig $ C.state0 p)
            <*> mapM (compileDefn conf) (C.loop p : C.state0 p : defns p))

