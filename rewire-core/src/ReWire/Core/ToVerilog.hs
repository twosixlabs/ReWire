{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Annotation (noAnn, ann)
import ReWire.BitVector (width, bitVec, BV, zeros, ones, lsb1, (==.), (@.), szBitRep)
import ReWire.Config (Config, ResetFlag (..))
import ReWire.Core.Interp (patApply', patMatches', subRange, dispatchWires, pausePrefix, extraWires, resumptionSize, Wiring')
import ReWire.Core.Mangle (mangle)
import ReWire.Core.Syntax as C hiding (Name, Size, Index)
import ReWire.Error (failAt, AstError, MonadError)
import ReWire.Fix (fix')
import ReWire.Pretty (prettyPrint, showt)
import ReWire.Verilog.Syntax as V

import qualified ReWire.BitVector as BV

import Control.Arrow ((&&&), first, second)
import Control.Lens ((^.), (.~))
import Control.Monad (liftM2)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.State (MonadState, runStateT, modify, gets)
import Data.Char (isAlphaNum)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T
import qualified ReWire.Config       as C

data FreshMode = FreshInit | FreshRun
type Fresh = (FreshMode, HashMap Text Int)
type DefnMap = HashMap GId (C.Exp, (Natural, Bool))
type SigInfo = (Fresh, [Signal])

freshInit0 :: Fresh
freshInit0 = (FreshInit, mempty)

freshRun0 :: Fresh
freshRun0 = (FreshRun, mempty)

mangleFresh :: Text -> Text
mangleFresh x = if isVerilogId x' then x' else mangle x'
      where subDots :: Text -> Text
            subDots = T.replace "." "_"

            subDollar :: Text -> Text
            subDollar = \ case
                  (T.stripPrefix "$" -> Just x') -> "Z" <> x'
                  x                              -> x

            subTick :: Text -> Text
            subTick = T.replace "'" "$"

            isVerilogId :: Text -> Bool
            isVerilogId = T.all isVerilogId'

            isVerilogId' :: Char -> Bool
            isVerilogId' c = isAlphaNum c || c == '_' || c == '$'

            x' :: Text
            x' = subTick $ subDollar $ subDots x

-- | Module names need to be de-conflicted because they aren't immediately
--   freshened.
mangleMod :: Text -> Text
mangleMod x = mangleFresh x'
      where subDots :: Text -> Text
            subDots = T.replace "_" "__"

            subDollar :: Text -> Text
            subDollar = \ case
                  (T.stripPrefix "Z" -> Just x') -> "ZZ" <> x'
                  x                              -> x

            x' :: Text
            x' = subDollar $ subDots x

fresh' :: MonadState SigInfo m => Text -> m Name
fresh' s = do
      (mode, ctrs) <- gets fst
      let ctr = fromMaybe 0 $ Map.lookup s ctrs
      modify $ first $ second $ Map.insert s (ctr + 1)
      pure $ s <> modeTag mode <> ctrTag ctr
      where modeTag :: FreshMode -> Text
            modeTag = \ case
                  FreshInit -> "S0"
                  _         -> mempty

            ctrTag :: Int -> Text
            ctrTag = \ case
                  n | n > 0 -> "R" <> showt n
                  _         -> mempty

fresh :: MonadState SigInfo m => Text -> m Name
fresh s = fresh' $ T.toLower $ mangleFresh s

newWire :: MonadState SigInfo m => V.Size -> Name -> m LVal
newWire sz n = do
      n' <- fresh n
      modify $ second (<> [mkSignal (n', sz)])
      pure $ Name n'

newWire' :: MonadState SigInfo m => V.Size -> Name -> m LVal
newWire' sz n = do
      n' <- fresh' n
      modify $ second (<> [mkSignal (n', sz)])
      pure $ Name n'

lookupWidth :: (MonadError AstError m, MonadState SigInfo m) => Name -> m (Maybe Size)
lookupWidth n = do
      sigs <- gets snd >>= mapM proj
      pure $ lookup n sigs
      where proj :: MonadError AstError m => Signal -> m (Name, Size)
            proj = \ case
                  Wire  []   n _ -> pure (n, 0)
                  Wire  [sz] n _ -> pure (n, sz)
                  Logic []   n _ -> pure (n, 0)
                  Logic [sz] n _ -> pure (n, sz)
                  Reg   []   n _ -> pure (n, 0)
                  Reg   [sz] n _ -> pure (n, sz)
                  _              -> failAt noAnn $ "toVerilog: lookupWidth: unexpected multi-dimensional signal (rwc bug): " <> showt n

compileProgram :: (MonadFail m, MonadError AstError m) => Config -> C.Device -> m V.Device
compileProgram conf p@(C.Device topLevel w loop state0 ds)
      | conf^.C.flatten = V.Device . pure <$> runReaderT (compileStart conf' topLevel w loop state0) defnMap'
      | otherwise       = flip runReaderT defnMap' $ do
            st' <- compileStart conf' topLevel w loop state0
            -- Initial state should be inlined, so we can filter out its defn.
            ds' <- mapM (compileDefn conf') $ filter (inuse . defnName) $ loop : ds
            pure $ V.Device $ st' : ds'
      where defnMap' :: DefnMap
            defnMap' = Map.mapKeys mangleMod $ defnMap p

            inuse :: GId -> Bool
            inuse g = case Map.lookup g $ defnUses p of
                  Nothing -> False
                  Just 0  -> False
                  Just 1  -> False
                  _       -> True

            noClockedMods :: Bool
            noClockedMods = Map.null $ Map.filter (not . snd . snd) defnMap'

            conf' :: Config
            conf' = C.clock.~clock' $ C.reset.~reset' $ conf

            clock' :: Text
            clock' | null (dispatchWires w')
                   , noClockedMods = mempty
                   | otherwise     = conf^.C.clock

            reset' :: Text
            reset' | T.null clock' = mempty
                   | otherwise     = conf^.C.reset

            w' :: Wiring'
            w' = (w, defnSig loop, defnSig state0)

compileStart :: (MonadError AstError m, MonadFail m, MonadReader DefnMap m)
                 => Config -> Name -> C.Wiring -> C.Defn -> C.Defn -> m Module
compileStart conf topLevel w loop state0 = do
      ((rStart, ssStart), (_, startSigs)) <- flip runStateT (freshInit0, [])
            $ compileCall (C.flatten.~True $ conf) (mangleMod $ defnName state0) (resumptionSize w') []

      ((rLoop, ssLoop),   (_, loopSigs))  <- flip runStateT (freshRun0, [])
            $ compileCall conf (mangleMod $ defnName loop) (resumptionSize w')
                  $  [ V.cat $ map (LVal . Name . fst) $ dispatchWires w' | dispatchSz > 0 ]
                  <> [ V.cat $ map (LVal . Name . fst) $ inputWires w     | inputSz > 0 ]

      let mod                    = Module topLevel $ inputs <> outputs
          loopStmts | loopSz > 0 = ssLoop <> [ Assign lvPause rLoop ]
                    | otherwise  = []

      -- If dispatchSz is 0, the top_level doesn't need the clock, but might
      -- need the clock signal for sub-modules.
      if dispatchSz == 0 then pure $ mod (loopSigs <> sigs) loopStmts
      else do
            initExp <- initState rStart
            pure $ mod (loopSigs <> startSigs <> sigs)
                 $  ssStart <> loopStmts
                 <> [ Initial $ ParAssign lvCurrState initExp
                    , Always (Pos (conf^.C.clock) : rstEdge) $ Block [ ifRst initExp ]
                    ]

      where clockSig :: Maybe Signal
            clockSig | T.null (conf^.C.clock) = Nothing
                     | otherwise              = pure $ mkSignal (conf^.C.clock, 1)

            resetSig :: Maybe Signal
            resetSig | T.null (conf^.C.reset) = Nothing
                     | otherwise              = pure $ mkSignal (conf^.C.reset, 1)

            loopSz :: Size
            loopSz | Sig _ _ sz <- defnSig loop = sz

            inputs :: [Port]
            inputs = map Input $ catMaybes [clockSig, resetSig] <> map mkSignal (inputWires w)

            outputs :: [Port]
            outputs = map (Output . mkSignal) $ outputWires w

            sigs :: [Signal]
            sigs = map mkSignal allWires

            ifRst :: V.Exp -> Stmt
            ifRst init = case resetSig of
                  Just (Logic [sz] sRst _) -> IfElse (V.Eq (LVal $ Name sRst) $ LitBits $ bitVec (fromIntegral sz) $ fromEnum $ not invertRst)
                        (Block [ ParAssign lvCurrState init ] )
                        (Block [ ParAssign lvCurrState $ LVal lvNxtState ])
                  _            -> ParAssign lvCurrState $ LVal lvNxtState

            dispatchSz :: Size
            dispatchSz = sum $ snd <$> dispatchWires w'

            inputSz :: Size
            inputSz = sum $ snd <$> inputWires w

            -- | Initial/reset state.
            initState :: MonadError AstError m => V.Exp -> m V.Exp
            initState e = case (expToBV e, fromIntegral dispatchSz) of
                  (Just bv, n) | n > 0 -> pure $ bvToExp $ subRange (0, n - 1) bv
                  (_, n)       | n > 0 -> pure $ bvToExp $ zeros n -- TODO(chathhorn): make configurable?
                  (_, 0)               -> pure V.nil
                  st                   -> failAt noAnn $ "compileStart: could not calculate initial state: " <> showt st

            lvPause :: LVal
            lvPause = mkLVals $ Name . fst <$> pauseWires

            lvCurrState :: LVal
            lvCurrState = mkLVals $ Name . fst <$> dispatchWires w'

            lvNxtState :: LVal
            lvNxtState = mkLVals $ Name . fst <$> nextDispatchWires

            rstEdge :: [Sensitivity]
            rstEdge | T.null (conf^.C.reset) = []
                    | syncRst                = []
                    | invertRst              = [Neg $ conf^.C.reset]
                    | otherwise              = [Pos $ conf^.C.reset]

            invertRst :: Bool
            invertRst = Inverted `elem` (conf^.C.resetFlags)

            syncRst :: Bool
            syncRst = Synchronous `elem` (conf^.C.resetFlags)

            pauseWires :: [(Name, Size)]
            pauseWires = pausePrefix w' <> nextDispatchWires

            nextDispatchWires :: [(Name, Size)]
            nextDispatchWires = first (<> "_next") <$> dispatchWires w'

            allWires :: [(Name, Size)]
            allWires = extraWires w' <> dispatchWires w' <> nextDispatchWires

            w' :: Wiring'
            w' = (w, defnSig loop, defnSig state0)

compileDefn :: (MonadFail m, MonadError AstError m, MonadReader DefnMap m) => Config -> C.Defn -> m V.Module
compileDefn conf (C.Defn _ n (Sig _ inps outp) body) = do
      ((e, stmts), (_, sigs)) <- flip runStateT (freshRun0, []) $ compileExp conf (map (LVal . Name) argNames) body
      isPure' <- isPureDefn n
      let inputs' = if isPure' then inputs else [clkPort, rstPort] <> inputs
      pure $ V.Module (mangleMod n) (inputs' <> outputs) sigs $ stmts <> [Assign (Name "res") e]
      where argNames :: [Name]
            argNames = zipWith (\ _ x -> "arg" <> showt x) inps [0::Int ..]

            inputs :: [Port]
            inputs = zipWith (curry $ Input . mkSignal) argNames inps

            outputs :: [Port]
            outputs = map (Output . mkSignal) [("res", outp)]

            clkPort :: Port
            clkPort = Input $ Logic [1] (conf^.C.clock) []

            rstPort :: Port
            rstPort = Input $ Logic [1] (conf^.C.reset) []

            isPureDefn :: MonadReader DefnMap m => GId -> m Bool
            isPureDefn g = asks (Map.lookup $ mangleMod g) >>= \ case
                  Just (_, (_, b)) -> pure b
                  _                -> pure False

-- | Inlines a defn or instantiates an already-compiled defn.
compileCall :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
             => Config -> GId -> V.Size -> [V.Exp] -> m (V.Exp, [Stmt])
compileCall conf g sz lvars = asks (Map.lookup g) >>= \ case
      Just (body, (uses, _)) | uses == 1 || conf^.C.flatten -> do
            (e, stmts) <- compileExp conf lvars body
            e'         <- wcast sz e
            pure (e', stmts)
      Just (_, (_, isPure'))                              -> do
            mr         <- newWire sz $ g <> "_out"
            inst'      <- fresh' "inst"
            let stmt   = Instantiate g inst' []
                       $ zip (repeat mempty)
                       $ (if isPure' then [] else [clk, rst]) <> lvars <> [LVal mr]
            pure (LVal mr, [stmt])
      _ -> failAt noAnn $ "ToVerilog: compileCall: failed to find definition for " <> g <> " while flattening."
      where clk :: V.Exp
            clk = LVal $ Name $ conf^.C.clock

            rst :: V.Exp
            rst = LVal $ Name $ conf^.C.reset

instantiate :: (MonadFail m, MonadState SigInfo m, MonadError AstError m) => Config -> ExternSig -> GId -> Text -> V.Size -> [V.Exp] -> m (V.Exp, [Stmt])
instantiate conf (ExternSig an ps theirClock theirReset args res) g inst sz lvars = do
      Name mr         <- newWire sz "extRes"
      (args', lvars') <- addReset (args, lvars) >>= addClock
      inst'           <- fresh' $ if T.null inst then "inst" else inst
      let stmt = Instantiate g inst' (map (second $ LitBits . bitVec 32) ps)
               $ zip (fst <$> args') lvars' <> zip (fst <$> res) (toSubRanges mr (snd <$> res))
      pure (LVal $ Name mr, [stmt])
      where clk :: V.Exp
            clk = LVal $ Name $ conf^.C.clock

            rst :: V.Exp
            rst = LVal $ Name $ conf^.C.reset

            addClock :: MonadError AstError m => ([(Text, Size)], [V.Exp]) -> m ([(Text, Size)], [V.Exp])
            addClock (args, lvars)
                  | T.null theirClock            = pure (args, lvars)
                  | not (T.null $ conf^.C.clock) = pure ((theirClock, 1) : args, clk : lvars)
                  | otherwise                    = failAt an "ToVerilog: external module requires a clock signal, but we have no clock to give it."

            addReset :: MonadError AstError m => ([(Text, Size)], [V.Exp]) -> m ([(Text, Size)], [V.Exp])
            addReset (args, lvars)
                  | T.null theirReset            = pure (args, lvars)
                  | not (T.null $ conf^.C.reset) = pure ((theirReset, 1) : args, rst : lvars)
                  | otherwise                    = failAt an "ToVerilog: external module requires a reset signal, but we have no reset to give it."


compileExps :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => Config -> [V.Exp] -> [C.Exp] -> m ([V.Exp], [Stmt])
compileExps conf lvars es = (map fst &&& concatMap snd) <$> mapM (compileExp conf lvars) es

compileExp :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => Config -> [V.Exp] -> C.Exp -> m (V.Exp, [Stmt])
compileExp conf lvars = \ case
      LVar _  _ (lkupLVal -> Just x)                -> pure (x, [])
      LVar an _ _                                   -> failAt an "ToVerilog: compileExp: encountered unknown LVar."
      Lit _ bv                                      -> pure (bvToExp bv, [])
      C.Concat _ e1 e2                              -> first V.cat <$> compileExps conf lvars (gather e1 <> gather e2)
      Call _ sz (Global g) e ps els                 -> mkCall g e ps els $ compileCall conf (mangleMod g) sz
      Call an sz (SetRef r) e ps els                -> mkCall2' "setRef" e ps els $ \ (a, b) -> case a of
            a@(LVal (Element _ _)) -> do -- TODO(chathhorn) TODO TODO
                  let wa  = 1
                  r' <- newWire' wa r
                  b' <- wcast sz b
                  pure (b', [Assign r' a])
            a@(LVal (Range _ i j)) -> do -- TODO(chathhorn) TODO TODO
                  let wa  = fromIntegral $ j - i + 1
                  r' <- newWire' wa r
                  b' <- wcast sz b
                  pure (b', [Assign r' a])
            LVal (Name a) -> do
                  wa  <- fromMaybe 0 <$> lookupWidth a
                  r' <- newWire' wa r
                  b' <- wcast sz b
                  pure (b', [Assign r' $ LVal $ Name a])
            a -> failAt an $ "ToVerilog: setRef: encountered unsupported expression: " <> prettyPrint a
      Call _ sz (GetRef r) _ _ _                   -> (,[]) <$> wcast sz (LVal $ Name r)
      Call _ sz (Prim (binOp -> Just op)) e ps els -> mkCall2 "binOp"     e ps els $ \ (x, y) -> wcast sz (op x y)
      Call _ sz (Prim (unOp -> Just op)) e ps els  -> mkCall1 "unOp"      e ps els $ \ x      -> wcast sz (op x)
      Call _ sz (Prim MSBit) e ps els              -> mkCall1 "msbit"     e ps els $ \ x      -> wcast sz (projBit (fromIntegral (argsSize ps) - 1) x)
      Call _ sz (Prim Resize) e ps els             -> mkCall1 "resize"    e ps els $ \ x      -> wcast sz x
      Call _ sz (Prim Id) e ps els                 -> mkCall "id"         e ps els $ \ xs     -> (,[]) <$> wcast sz (V.cat xs)
      Call _ sz (Prim Reverse) e ps els            -> mkCall "reverse"    e ps els $ \ xs     -> (,[]) <$> wcast sz (V.cat $ reverse xs)
      Call _ sz (Prim (Replicate n)) e ps els      -> mkCall1 "replicate" e ps els $ \ x      -> wcast sz (if n > 0 then V.Repl (toLit n) x else V.nil)
      Call a _  (Prim p) _ _ _                     -> failAt a $ "ToVerilog: compileExp: encountered unknown primitive: " <> showt p
      Call _ sz (Extern sig ex inst) e ps els      -> mkCall ex          e ps els $ instantiate conf sig ex inst sz
      Call _ sz (Const bv) e ps els                -> mkCall "lit"       e ps els $ \ _      -> (,[]) <$> wcast sz (bvToExp bv)
      where mkCall2 :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> ((V.Exp, V.Exp) -> m V.Exp) -> m (V.Exp, [Stmt])
            mkCall2 s e ps els f = mkCall2' s e ps els $ fmap (, []) . f

            mkCall2' :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> ((V.Exp, V.Exp) -> m (V.Exp, [V.Stmt])) -> m (V.Exp, [Stmt])
            mkCall2' s e ps els f = mkCall s e ps els $ \ case
                  [e1, e2] -> f (e1, e2)
                  es       -> failAt (ann e) $ "primitive " <> s <> ": expected two arguments, got: " <> showt (length es)

            mkCall1 :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> (V.Exp -> m V.Exp) -> m (V.Exp, [Stmt])
            mkCall1 s e ps els f = mkCall s e ps els $ \ case
                  [e1] -> (, []) <$> f e1
                  es   -> failAt (ann e) $ "primitive " <> s <> ": expected one argument, got: " <> showt (length es)

            mkCall :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> ([V.Exp] -> m (V.Exp, [Stmt])) -> m (V.Exp, [Stmt])
            mkCall s e ps els f = do
                  (e', stmts)    <- compileExp conf lvars e
                  (els', stmts') <- compileExp conf lvars els
                  case litVal e' of
                        Just bv -> do
                              (fes, fstmts) <- f $ patApplyLit bv ps
                              pure (if patMatchesLit bv ps then fes else els', stmts <> stmts' <> fstmts)
                        _       -> do
                              Name n <- newWire (sizeOf e) $ s <> "_in"
                              (fes, fstmts) <- f $ map LVal $ patApply n ps
                              pure  ( let c = patMatches n ps in
                                      if | litTrue c || C.isNil els -> fes
                                         | litFalse c               -> els'
                                         | otherwise                -> Cond c fes els'
                                    , stmts <> stmts' <> [ Assign (Name n) e' ] <> fstmts
                                    )

            litTrue :: V.Exp -> Bool
            litTrue e = case litVal e of
                  Just v  -> v /= zeros 1
                  Nothing -> False

            litFalse :: V.Exp -> Bool
            litFalse e = case litVal e of
                  Just v  -> v == zeros 1
                  Nothing -> False

            litVal :: V.Exp -> Maybe BV
            litVal = \ case
                  LitBits b -> Just b
                  _         -> Nothing

            lkupLVal :: LId -> Maybe V.Exp
            lkupLVal = flip lookup (zip [0::LId ..] lvars)

            projBit :: Index -> V.Exp -> V.Exp
            projBit ix = \ case
                  LVal (Range n i _)     -> LVal $ Element n $ ix + i
                  LVal (Name n)          -> LVal $ Element n ix
                  LitBits bv | bv @. ix  -> LitBits $ ones 1
                             | otherwise -> LitBits $ zeros 1
                  e                      -> e -- TODO(chathhorn): kludgy.

-- | Attempt to break up giant literals.
bvToExp :: BV -> V.Exp
bvToExp bv | width bv < maxLit     = LitBits bv
           | width bv == 0         = V.nil
           | bv == zeros 1         = Repl (toLit $ fromIntegral $ width bv) $ LitBits (zeros 1)
           | bv == ones (width bv) = Repl (toLit $ fromIntegral $ width bv) $ LitBits (ones 1)
           | zs > 8                = V.cat [LitBits $ subRange (fromIntegral zs, width bv - 1) bv, Repl (toLit zs) $ LitBits $ zeros 1]
           | otherwise             = LitBits bv -- TODO(chathhorn)
      where zs :: Natural -- trailing zeros
            zs | bv == zeros 1 = fromIntegral $ width bv
               | otherwise     = fromIntegral $ lsb1 bv

            maxLit :: Int
            maxLit = 32

toLit :: Natural -> V.Exp
toLit v = LitBits $ bitVec (fromIntegral $ szBitRep v) v

expToBV :: V.Exp -> Maybe BV
expToBV = \ case
      LitBits bv                 -> Just bv
      Repl (expToBV -> Just n) e -> BV.replicate (BV.nat n) <$> expToBV e
      V.Concat es                -> BV.concat <$> mapM expToBV es
      _                          -> Nothing

-- | Size of the inclusive range [i, j].
szRange :: Index -> Index -> Size
szRange i j = fromIntegral (j - i + 1)

mkLVals :: [V.LVal] -> V.LVal
mkLVals = \ case
      [e] -> e
      es  -> LVals es

mkRange :: Name -> Index -> Index -> V.LVal
mkRange n i j | i == j    = Element n i
              | otherwise = Range n i j

mkWCast :: Size -> V.Exp -> V.Exp
mkWCast sz = \ case
      LVal (Range n i j) | sz < szRange i j -> LVal $ mkRange n i (j - fromIntegral (szRange i j - sz))
      WCast _ e                             -> WCast sz e
      e                                     -> WCast sz e

-- | Recreates the effect of assignment by adding bitwidth casts to an expression:
-- > wcast s e
-- should return an e' that behaves the same as though e appeared as
-- > x = e
-- where x has a width of s. I.e., "wcast s e" fixes the "bitwidth context" for
-- the expression e to s.
wcast :: (MonadError AstError m, MonadState SigInfo m) => Size -> V.Exp -> m V.Exp
wcast sz e = expWidth e >>= \ case
      Just sz' | sz < sz' -> trunc e
               | sz > sz' -> case e of
                  V.Add  e1 e2      -> V.Add         <$> pad e1 <*> pad e2
                  V.Sub  e1 e2      -> V.Sub         <$> pad e1 <*> pad e2
                  V.Mul  e1 e2      -> V.Mul         <$> pad e1 <*> pad e2
                  V.Div  e1 e2      -> V.Div         <$> pad e1 <*> pad e2
                  V.Mod  e1 e2      -> V.Mod         <$> pad e1 <*> pad e2
                  V.And  e1 e2      -> V.And         <$> pad e1 <*> pad e2
                  V.Or   e1 e2      -> V.Or          <$> pad e1 <*> pad e2
                  V.XOr  e1 e2      -> V.XOr         <$> pad e1 <*> pad e2
                  V.XNor e1 e2      -> V.XNor        <$> pad e1 <*> pad e2
                  V.Cond c e1 e2    -> V.Cond c      <$> pad e1 <*> pad e2
                  V.Pow         e n -> V.Pow         <$> pad e  <*> pure n
                  V.LShift      e n -> V.LShift      <$> pad e  <*> pure n
                  V.RShift      e n -> V.RShift      <$> pad e  <*> pure n
                  V.LShiftArith e n -> V.LShiftArith <$> pad e  <*> pure n
                  V.RShiftArith e n -> V.RShiftArith <$> pad e  <*> pure n
                  V.Not e           -> V.Not         <$> pad e
                  e                 -> pad e
      _                   -> pure e
      where pad :: (MonadError AstError m, MonadState SigInfo m) => V.Exp -> m V.Exp
            pad e = expWidth e >>= \ case
                        Just sz' | sz > sz' -> pure $ mkWCast sz e
                        _                   -> pure e
            trunc :: (MonadError AstError m, MonadState SigInfo m) => V.Exp -> m V.Exp
            trunc e = expWidth e >>= \ case
                        Just sz' | sz < sz' -> pure $ mkWCast sz e
                        _                   -> pure e

-- Expression                     Bit Length         Notes
-- -----------------------------------------------------------------------------
-- Unsized constants              "Same as integer"  (see ** below)
-- Sized constants                As given
-- i [+ - * / % & | ^ ^~ ~^] j    max{L(i),L(j)}
-- [+ - ~] i                      L(i)
-- i [=== !== == != > >= < <=] j  1 bit              i,j sized to max(L(i),L(j))
-- i [&& ||] j                    1 bit              i,j self-determined
-- [& ~& | ~| ^ ~^ ^~ !] i        1 bit              i self-determined
-- i [>> << ** >>> <<<] j         L(i)               j self-determined
-- i ? j : k                      max(L(j),L(k))     i self-determined
-- {i, ..., j}                    L(i)+...+L(j)      all self-determined
-- {i {j, ..., k}}                i*(L(j)+...+L(k))  all self-determined
-- -----------------------------------------------------------------------------
expWidth :: (MonadError AstError m, MonadState SigInfo m) => V.Exp -> m (Maybe Size)
expWidth = \ case
      V.Add    e1 e2                -> largest e1 e2
      V.Sub    e1 e2                -> largest e1 e2
      V.Mul    e1 e2                -> largest e1 e2
      V.Div    e1 e2                -> largest e1 e2
      V.Mod    e1 e2                -> largest e1 e2
      V.And    e1 e2                -> largest e1 e2
      V.Or     e1 e2                -> largest e1 e2
      V.XOr    e1 e2                -> largest e1 e2
      V.XNor   e1 e2                -> largest e1 e2
      V.Cond _ e1 e2                -> largest e1 e2
      V.Pow         e _             -> expWidth e
      V.LShift      e _             -> expWidth e
      V.RShift      e _             -> expWidth e
      V.LShiftArith e _             -> expWidth e
      V.RShiftArith e _             -> expWidth e
      V.Not         e               -> expWidth e
      V.LAnd _ _                    -> pure $ Just 1
      V.LOr _ _                     -> pure $ Just 1
      V.LNot _                      -> pure $ Just 1
      V.RAnd _                      -> pure $ Just 1
      V.RNAnd _                     -> pure $ Just 1
      V.ROr _                       -> pure $ Just 1
      V.RNor _                      -> pure $ Just 1
      V.RXOr _                      -> pure $ Just 1
      V.RXNor _                     -> pure $ Just 1
      V.Eq  _ _                     -> pure $ Just 1
      V.NEq _ _                     -> pure $ Just 1
      V.CEq _ _                     -> pure $ Just 1
      V.CNEq _ _                    -> pure $ Just 1
      V.Lt _ _                      -> pure $ Just 1
      V.Gt _ _                      -> pure $ Just 1
      V.LtEq _ _                    -> pure $ Just 1
      V.GtEq _ _                    -> pure $ Just 1
      V.Concat es                   -> sumMaybes <$> mapM expWidth es
      V.Repl (expToBV -> Just sz) e -> fmap (* fromIntegral (BV.nat sz)) <$> expWidth e
      V.Repl (expToBV -> Nothing) _ -> pure Nothing
      V.WCast sz _                  -> pure $ Just sz
      V.LitBits bv                  -> pure $ Just $ fromIntegral $ width bv
      V.LVal lv                     -> lvalWidth lv
      e                             -> failAt noAnn ("ToVerilog: expWidth: unsupported expression (rwc bug): " <> prettyPrint e)
      where largest :: (MonadError AstError m, MonadState SigInfo m) => V.Exp -> V.Exp -> m (Maybe Size)
            largest e1 e2 = liftM2 max <$> expWidth e1 <*> expWidth e2

            lvalWidth :: (MonadError AstError m, MonadState SigInfo m) => LVal -> m (Maybe Size)
            lvalWidth = \ case
                  Element _ _ -> pure $ Just 1
                  Range _ i j -> pure $ Just $ fromIntegral $ szRange i j
                  Name n      -> lookupWidth n
                  LVals lvs   -> sumMaybes <$> mapM lvalWidth lvs

sumMaybes :: Num a => [Maybe a] -> Maybe a
sumMaybes = foldMaybes (+)

foldMaybes :: (a -> a -> a) -> [Maybe a] -> Maybe a
foldMaybes f = \ case
      a : b : ms -> foldMaybes f $ (f <$> a <*> b) : ms
      [a]        -> a
      _          -> Nothing

binOp :: Prim -> Maybe (V.Exp -> V.Exp -> V.Exp)
binOp = flip lookup primBinOps

unOp :: Prim -> Maybe (V.Exp -> V.Exp)
unOp = flip lookup primUnOps

primBinOps :: [(Prim, V.Exp -> V.Exp -> V.Exp)]
primBinOps =
      [ ( C.Add         , V.Add)
      , ( C.Sub         , V.Sub)
      , ( C.Mul         , V.Mul)
      , ( C.Div         , V.Div)
      , ( C.Mod         , V.Mod)
      , ( C.Pow         , V.Pow)
      , ( C.LAnd        , V.LAnd)
      , ( C.LOr         , V.LOr)
      , ( C.And         , V.And)
      , ( C.Or          , V.Or)
      , ( C.XOr         , V.XOr)
      , ( C.XNor        , V.XNor)
      , ( C.LShift      , V.LShift)
      , ( C.RShift      , V.RShift)
      , ( C.RShiftArith , V.RShiftArith)
      , ( C.Eq          , V.Eq)
      , ( C.Gt          , V.Gt)
      , ( C.GtEq        , V.GtEq)
      , ( C.Lt          , V.Lt)
      , ( C.LtEq        , V.LtEq)
      ]

primUnOps :: [(Prim, V.Exp -> V.Exp)]
primUnOps =
      [ ( C.LNot  , V.LNot)
      , ( C.Not   , V.Not)
      , ( C.RAnd  , V.RAnd)
      , ( C.RNAnd , V.RNAnd)
      , ( C.ROr   , V.ROr)
      , ( C.RNor  , V.RNor)
      , ( C.RXOr  , V.RXOr)
      , ( C.RXNor , V.RXNor)
      ]

-- | Returns a boolean expression that is true when the pattern matches.
patMatches :: Name -> [Pat] -> V.Exp
patMatches x ps =  case patMatches' (\ i j bv -> [V.Eq (LVal $ mkRange x i j) $ LitBits bv]) ps of
      ps'@(_ : _) -> foldr1 V.LAnd ps'
      []          -> bTrue

patMatchesLit :: BV -> [Pat] -> Bool
patMatchesLit bv = and . patMatches' (\ i j bv' -> [subRange (i, j) bv ==. bv'])

-- | Returns a list of ranges bound by pattern variables.
patApply :: Name -> [Pat] -> [LVal]
patApply x = patApply' (\ i j -> [mkRange x i j])

patApplyLit :: BV -> [Pat] -> [V.Exp]
patApplyLit bv = patApply' (\ i j -> [LitBits $ subRange (i, j) bv])

toSubRanges :: Name -> [Size] -> [V.Exp]
toSubRanges n = patApply' (\ i j -> [LVal $ mkRange n i j]) . map (PatVar noAnn)

argsSize :: [Pat] -> Size
argsSize = sum . map patToSize
      where patToSize :: Pat -> Size
            patToSize = \ case
                  PatVar _ sz -> sz
                  _           -> 0

mkSignal :: (Name, Size) -> Signal
mkSignal (n, sz) = Logic [sz] n []

type Uses   = Natural
type IsPure = Bool

defnMap :: C.Device -> HashMap GId (C.Exp, (Uses, IsPure))
defnMap p@C.Device { loop, state0, defns } = foldl' defnInfo mempty defns'
      where defnInfo :: HashMap GId (C.Exp, (Uses, IsPure)) -> C.Defn -> HashMap GId (C.Exp, (Uses, IsPure))
            defnInfo m (Defn _ g _ e) = Map.insert g (e, (Map.findWithDefault 0 g uses, Set.member g pures)) m

            uses :: HashMap GId Uses
            uses = defnUses p

            pures :: HashSet GId
            pures = pureDefns p

            defns' :: [Defn]
            defns' = loop : state0 : defns

-- | Defns that do not require an implicit clock/reset.
pureDefns :: C.Device -> HashSet GId
pureDefns C.Device { loop, state0, defns } = fix' purity mempty
      where purity :: HashSet GId -> HashSet GId
            purity m = foldl' purity' m defns'

            purity' :: HashSet GId -> Defn -> HashSet GId
            purity' ps (Defn _ g _ e) = if isPure ps e then Set.insert g ps else ps

            defns' :: [Defn]
            defns' = loop : state0 : defns

defnUses :: C.Device -> HashMap GId Uses
defnUses C.Device { loop, state0, defns } = Map.fromList [(defnName loop, 1), (defnName state0, 1)]
      <+> foldr (<+>) Map.empty (expUses . defnBody <$> loop : defns) -- drop state0 body.
      where expUses :: C.Exp -> HashMap GId Uses
            expUses = \ case
                  C.Concat _ e1 e2              -> expUses e1 <+> expUses e2
                  C.Call _ _ (Global g) e _ els -> Map.singleton g 1 <+> expUses e <+> expUses els
                  C.Call _ _ _          e _ els ->                       expUses e <+> expUses els
                  _                             -> Map.empty

            (<+>) :: HashMap GId Uses -> HashMap GId Uses -> HashMap GId Uses
            (<+>) = Map.unionWith (+)

isPure :: HashSet GId -> C.Exp -> Bool
isPure m = \ case
      C.Call _ _ (C.Extern (C.ExternSig _ _ c r _ _) _ _) a _ b
                                    -> T.null c && T.null r && pur a && pur b
      C.Call _ _ (C.Global g) a _ b -> purG g && pur a && pur b
      C.Call _ _ _ a _ b            -> pur a && pur b
      C.Concat _ a b                -> pur a && pur b
      C.LVar {}                     -> True
      C.Lit {}                      -> True
      where pur :: C.Exp -> Bool
            pur = isPure m

            purG :: GId -> Bool
            purG = flip Set.member m

