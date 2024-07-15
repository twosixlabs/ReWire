module Main (main) where

import qualified RWC

import Control.Monad (unless, msum)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Directory (listDirectory, setCurrentDirectory)
import System.Environment (getArgs)
import System.Environment (withArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (callCommand)
import Test.Framework (defaultMainWithArgs, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Paths_rewire (getDataFileName)

data Flag = FlagV
          | FlagVhdl
          | FlagNoGhdl
          | FlagNoGhc
          | FlagNoDTypes
          | FlagChecker String
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
       [ Option ['v'] ["verbose"]      (NoArg FlagV)                  "More verbose output."
       , Option []    ["vhdl"]         (NoArg FlagVhdl)               "Test VHDL code generation in addition to Verilog."
       , Option []    ["no-ghdl"]      (NoArg FlagNoGhdl)             "Implies --vhdl: disable verification of output VHDL with 'ghdl -s' (which requires 'ghdl' in $PATH)."
       , Option []    ["no-ghc"]       (NoArg FlagNoGhc)              "Disable running tests through ghc."
       , Option []    ["vhdl-checker"] (ReqArg FlagChecker "command") "Set the command to use for checking generated VHDL (default: 'ghdl -s')."
       ]

testCompiler :: [Flag] -> FilePath -> [Test]
testCompiler flags fn =
      -- Test: compile Haskell source with GHC
      if FlagNoGhc `elem` flags then []
      else [ testCase (takeBaseName fn ++ " (stack ghc)") $ do
            setCurrentDirectory $ takeDirectory fn
            callCommand $ "stack ghc " ++ fn
      ] <>
      -- Test: compile Haskell to Verilog with RWC.
      [ testCase (takeBaseName fn) $ do
            setCurrentDirectory $ takeDirectory fn
            withArgs (fn : extraFlags) RWC.main
      ] <>
      -- Test: compile Haskell to VHDL with RWC.
      if not (FlagVhdl `elem` flags || FlagNoGhdl `elem` flags) then []
      else [ testCase (takeBaseName fn <> " (VHDL output)") $ do
            setCurrentDirectory $ takeDirectory fn
            withArgs ("--vhdl": fn : extraFlags) RWC.main
      ] <>
      -- Test: check VHDL output with ghdl.
      if FlagNoGhdl `elem` flags then []
      else [ testCase (takeBaseName fn ++ " (" ++ vhdlCheck ++ ")") $ do
            setCurrentDirectory $ takeDirectory fn
            callCommand $ vhdlCheck ++ " " ++ fn -<.> "vhdl"
      ]
      where extraFlags :: [String]
            extraFlags = if FlagV `elem` flags then ["-v"] else []

            vhdlCheck :: String
            vhdlCheck = fromMaybe "ghdl -s" $ msum $ flip map flags $ \ case
                  FlagChecker c -> Just $ sq c
                  _             -> Nothing

sq :: String -> String
sq = \ case
      '"'  : s | last s == '"'  -> init s
      '\'' : s | last s == '\'' -> init s
      s                         -> s

getTests :: [Flag] -> FilePath -> IO Test
getTests flags dirName = do
      dir   <- getDataFileName ("tests" </> dirName)
      files <- map (dir </>) . filter (".hs" `isSuffixOf`) <$> listDirectory dir
      pure $ testGroup dirName $ concatMap (testCompiler flags) files

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc-test [OPTION...]" options) >> exitFailure

main :: IO ()
main = do
      (flags, testDirs, errs) <- getOpt Permute options <$> getArgs

      let testDirs' = if null testDirs then ["regression"] else testDirs -- TODO(chathhorn): re-enable integration tests.

      unless (null errs) $ do
            mapM_ (hPutStrLn stderr) errs
            exitUsage

      tests <- mapM (getTests flags) testDirs'

      defaultMainWithArgs tests []
