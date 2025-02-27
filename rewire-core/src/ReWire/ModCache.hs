{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.ModCache
      ( runCache
      , getDevice
      , LoadPath
      , printInfo
      , printInfoHSE
      , printHeader
      ) where

import ReWire.Annotation (Annotation, SrcSpanInfo, unAnn)
import ReWire.Config (Config, loadPath, typecheck, pDebug)
import ReWire.Crust.KindCheck (kindCheck)
import ReWire.Crust.PrimBasis (addPrims)
import ReWire.Crust.Purify (purify)
import ReWire.Crust.Syntax (FreeProgram, Defn (..), Module (Module), Exp, Ty, Kind, DataConId, TyConId, Program (Program), prettyFP)
import ReWire.Crust.ToCore (toCore)
import ReWire.Crust.Transform (removeMain, simplify, liftLambdas, etaAbsDefs, shiftLambdas, neuterExterns, expandTypeSynonyms, inlineAnnotated, normalizeBind, elimCase, purge, purgeAll, inlineExtrudes, reduce)
import ReWire.Crust.TypeCheck (typeCheck, untype)
import ReWire.Error (failAt, AstError, MonadError, filePath)
import ReWire.HSE.Annotate (annotate)
import ReWire.HSE.Desugar (desugar, addMainModuleHead)
import ReWire.HSE.Parse (tryParseInDir)
import ReWire.HSE.Rename (Exports, Renamer, fromImps, allExports, toFilePath, fixFixity)
import ReWire.HSE.ToCrust (extendWithGlobs, toCrust, getImps)
import ReWire.Pretty (prettyPrint, prettyPrint', showt)
import ReWire.Unbound (fv, trec, runFreshMT, FreshMT, Name, Fresh, s2n)

import Control.Lens ((^.))
import Control.Arrow ((***))
import Control.Monad ((>=>), msum, void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify, lift)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)
import Language.Haskell.Exts.Syntax hiding (Annotation, Exp, Module (..), Namespace, Name, Kind)
import Numeric.Natural (Natural)
import System.FilePath ((</>), takeDirectory)

import qualified Data.HashMap.Strict          as Map
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Language.Haskell.Exts.Pretty as P
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.Core.Syntax           as Core
import qualified ReWire.Config                as C

type Cache m = StateT ModCache (FreshMT m)
type LoadPath = [FilePath]
type ModCache = HashMap FilePath (Module, Exports)

runCache :: (MonadIO m, MonadError AstError m) => Cache m a -> m a
runCache m = fst <$> runFreshMT (runStateT m mempty)

mkRenamer :: (MonadFail m, MonadIO m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> S.Module SrcSpanInfo -> Cache m Renamer
mkRenamer conf pwd' m = extendWithGlobs m . mconcat <$> mapM mkRenamer' (getImps m)
      where mkRenamer' :: (MonadFail m, MonadIO m, MonadError AstError m, MonadState AstError m) => ImportDecl SrcSpanInfo -> Cache m Renamer
            mkRenamer' (ImportDecl _ (void -> m) quald _ _ _ (fmap void -> as) specs) = do
                  (_, exps) <- getModule conf pwd' $ toFilePath m
                  fromImps m quald exps as specs

-- Pass 1    Parse.
-- Pass 2-4  Fixity fixing (uniquify + fix + deuniquify, because bug in applyFixities).
-- Pass 5    Annotate.
-- Pass 6-14 Desugar.
-- Pass 15   Translate to crust + rename globals.
-- Pass 16   Translate to core

getModule :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FilePath -> Cache m (Module, Exports)
getModule conf pwd fp = pDebug conf ("Fetching module: " <> pack fp <> " (pwd: " <> pack pwd <> ")") >> Map.lookup fp <$> get >>= \ case
      Just p  -> pure p
      Nothing -> do
            modify $ Map.insert fp mempty

            let lp     = pwd : conf^.loadPath

            mmods      <- mapM (tryParseInDir fp) lp
            (pwd', m)  <- maybe
                              (failAt (filePath fp) "File not found in load-path")
                              (pure . (elideDot *** addMainModuleHead))
                        $ msum mmods

            rn         <- mkRenamer conf pwd' m
            imps       <- loadImports pwd' m

            let pass :: MonadIO m => Natural -> Text -> S.Module a -> m (S.Module a)
                pass = passHSE conf rn imps

            -- Phase 1 (haskell-src-exts) transformations.
            (m', exps) <- pure
                      >=> pass 1 "(Haskell) Fixing fixity."
                      >=> lift . lift . fixFixity rn
                      >=> pass 2 "(Haskell) Annotating."
                      >=> pure . annotate
                      >=> pass 3 "(Haskell) Desugaring."
                      >=> desugar rn
                      >=> pass 4 "(Haskell) Translating to Crust IR."
                      >=> toCrust rn
                      $ m

            let Module ts syns ds = m' <> imps
            _ <- passCrust conf 5 ("Concatenating Crust IR for module: " <> pack fp) (ts, syns, ds)

            modify $ Map.insert fp (m' <> imps, exps)
            pure (m' <> imps, exps)

      where loadImports :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m, Annotation a) => FilePath -> S.Module a -> Cache m Module
            loadImports pwd' = fmap mconcat . mapM (fmap fst . getModule conf pwd' . toFilePath . void . importModule) . getImps

            elideDot :: FilePath -> FilePath
            elideDot = \ case
                  "." -> takeDirectory fp
                  d   -> d </> takeDirectory fp

-- Phase 2 (pre-core) transformations.
getDevice :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> Cache m Core.Device
getDevice conf fp = do
      (Module ts syns ds,  _)  <- getModule conf "." fp

      p <- pure
       >=> pass 6 "Adding primitives"
       >=> pure . addPrims
       >=> pass 7 "Removing the Main.main definition (before attempting to typecheck it)."
       >=> pure . removeMain
       >=> pass 8 "Inlining INLINE-annotated definitions."
       >=> inlineAnnotated
       >=> pass 9 "Expanding type synonyms."
       >=> expandTypeSynonyms
       >=> pass 10 "Typechecking, inference."
       >=> kindCheck >=> typeCheck start
       >=> pass 11 "Removing Haskell definitions for externs."
       >=> pure . neuterExterns
       >=> pass 12 "Removing unused definitions."
       >=> purge start >=> extraTC
       >=> pass 13 "Eliminating pattern bindings (case expressions)."
       >=> elimCase >=> liftLambdas >=> extraTC
       >=> pass 14 "Partial evaluation."
       >=> simplify conf >=> extraTC
       >=> pass 15 "Normalizing bind."
       >=> normalizeBind >=> extraTC
       >=> pass 16 "Lifting, shifting, eta-abstracting lambdas."
       >=> liftLambdas >=> purge start >=> extraTC
       >=> inlineExtrudes >=> reduce >=> extraTC -- TODO: reduce here really just for start defn.
       >=> shiftLambdas >=> etaAbsDefs >=> extraTC
       >=> pass 17 "Purifying."
       -- TODO: typechecking before or after purify seems to subtly effect
       --       ordering of things.
       -- >=> verb "Mystery round of type-checking/inference."
       -- >=> kindCheck >=> typeCheck start
       >=> purify start >=> extraTC
       -- >=> verb "Mystery round of type-checking/inference."
       -- >=> kindCheck >=> typeCheck start
       >=> pass 18 "Final lifting, shifting, eta-abstracting lambdas."
       >=> liftLambdas >=> shiftLambdas >=> etaAbsDefs
       >=> pass 19 "Final purging of unused definitions."
       >=> purgeAll start
       >=> pass 20 "Translating to core & HDL."
       >=> toCore conf start
       >=> verb "[21] Core."
       $ (ts, syns, ds)

      when ((conf^.C.dump) 21) $ liftIO $ do
            printHeader "[21] Core"
            T.putStrLn $ prettyPrint p
            when (conf^.C.verbose) $ do
                  T.putStrLn "\n## Show core:\n"
                  T.putStrLn $ showt $ unAnn p

      pure p

      where start :: Name Exp
            start = s2n $ conf^.C.start

            extraTC :: (Fresh m, MonadIO m, MonadError AstError m) => FreeProgram -> m FreeProgram
            extraTC | conf^.typecheck = verb "Type-checking again (--debug-typecheck)." >=> kindCheck >=> typeCheck start
                    | otherwise       = pure

            pass :: MonadIO m => Natural -> Text -> FreeProgram -> m FreeProgram
            pass = passCrust conf

            verb :: MonadIO m => Text -> a -> m a
            verb = verb' conf

printHeader :: MonadIO m => Text -> m ()
printHeader hd = do
      liftIO $ T.putStrLn   "-- # ==================================================="
      liftIO $ T.putStrLn $ "-- # " <> hd
      liftIO $ T.putStrLn   "-- # ===================================================\n"

verb' :: MonadIO m => Config -> Text -> a -> m a
verb' conf s a = pDebug conf s >> pure a

passHSE :: MonadIO m => Config -> Renamer -> Module -> Natural -> Text -> S.Module a -> m (S.Module a)
passHSE conf rn imps n m = verb' conf msg
            >=> if (conf^.C.dump) n then printInfoHSE conf msg rn imps else pure
      where msg = "[" <> showt n <> "] " <> m

passCrust :: MonadIO m => Config -> Natural -> Text -> FreeProgram -> m FreeProgram
passCrust conf n m = verb' conf msg
            >=> if (conf^.C.dump) n then printInfo conf msg else pure
      where msg = "[" <> showt n <> "] " <> m

printInfo :: MonadIO m => Config -> Text -> FreeProgram -> m FreeProgram
printInfo conf hd fp = do
      let p = Program $ trec fp
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "-- ## Free kind vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name Kind]))
      when verbose $ liftIO $ T.putStrLn "-- ## Free type vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name Ty]))
      when verbose $ liftIO $ T.putStrLn "-- ## Free tycon vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name TyConId]))
      liftIO $ T.putStrLn "-- ## Free con vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name DataConId]))
      liftIO $ T.putStrLn "-- ## Free exp vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name Exp]))
      liftIO $ T.putStrLn "-- ## Program:\n"
      fp' <- purgeAll start fp
      liftIO . T.putStrLn $ prettyPrint' $ prettyFP $ if verbose then fp' else untype' fp'
      when verbose $ liftIO $ T.putStrLn "\n-- ## Program (show):\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ unAnn fp
      pure fp

      where untype' :: FreeProgram -> FreeProgram
            untype' (ts, syns, vs) = (ts, syns, map untype'' vs)

            untype'' :: Defn -> Defn
            untype'' d = d { defnBody = untype $ defnBody d }

            comVar :: Text -> Text
            comVar = (<> "\n") . ("-- " <>)

            verbose :: Bool
            verbose = conf^.C.verbose

            start :: Name Exp
            start = s2n $ conf^.C.start

printInfoHSE :: MonadIO m => Config -> Text -> Renamer -> Module -> S.Module a -> m (S.Module a)
printInfoHSE conf hd rn imps hse = do
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "\n-- ## Renamer:\n"
      when verbose $ liftIO $ T.putStrLn $ showt rn
      when verbose $ liftIO $ T.putStrLn "\n-- ## Exports:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ allExports rn
      when verbose $ liftIO $ T.putStrLn "\n-- ## Show imps:\n"
      when verbose $ liftIO $ T.putStrLn $ showt imps
      when verbose $ liftIO $ T.putStrLn "\n-- ## Show HSE mod:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ void hse
      when verbose $ liftIO $ T.putStrLn "\n-- ## Pretty HSE mod:\n"
      liftIO $ putStrLn $ P.prettyPrint $ void hse
      pure hse

      where verbose :: Bool
            verbose = conf^.C.verbose
