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
          | FlagNoCheck
          | FlagNoGhc
          | FlagNoDTypes
          | FlagVhdlChecker String
          | FlagVerilogChecker String
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
       [ Option ['v'] ["verbose"]         (NoArg FlagV)                         "More verbose output."
       , Option []    ["vhdl"]            (NoArg FlagVhdl)                      "Test VHDL code generation in addition to Verilog."
       , Option []    ["no-check"]        (NoArg FlagNoCheck)                   "Disable verification of output HDL with checker."
       , Option []    ["no-ghc"]          (NoArg FlagNoGhc)                     "Disable running tests through ghc."
       , Option []    ["vhdl-checker"]    (ReqArg FlagVhdlChecker "command")    "Set the command to use for checking generated VHDL (default: 'ghdl -s')."
       , Option []    ["verilog-checker"] (ReqArg FlagVerilogChecker "command") "Set the command to use for checking generated Verilog (default: 'iverilog -Wall -g2012')."
       ]

testCompiler :: [Flag] -> FilePath -> [Test]
testCompiler flags fn =
      -- Test: compile Haskell source with GHC
      (if FlagNoGhc `elem` flags then []
      else [ testCase (takeBaseName fn ++ " (stack ghc)") $ do
            setCurrentDirectory $ takeDirectory fn
            callCommand $ "stack ghc " ++ fn
      ])

      --- Verilog tests ---

      -- Test: compile Haskell to Verilog with RWC.
      <> [ testCase (takeBaseName fn) $ do
            setCurrentDirectory $ takeDirectory fn
            withArgs (fn : extraFlags) RWC.main
         ]

      -- Test: check Verilog output.
      <> (if FlagNoCheck `elem` flags then []
         else [ testCase (takeBaseName fn ++ " (" ++ verilogCheck ++ ")") $ do
            setCurrentDirectory $ takeDirectory fn
            callCommand $ verilogCheck ++ " " ++ fn -<.> "sv"
         ])

      --- VHDL tests ---

      -- Test: compile Haskell to VHDL with RWC.
      <> (if FlagVhdl `elem` flags
         then [ testCase (takeBaseName fn <> " (VHDL output)") $ do
            setCurrentDirectory $ takeDirectory fn
            withArgs ("--vhdl": fn : extraFlags) RWC.main
         ] else [])

      -- Test: check VHDL output.
      <> (if FlagVhdl `elem` flags && FlagNoCheck `notElem` flags
         then [ testCase (takeBaseName fn ++ " (" ++ vhdlCheck ++ ")") $ do
            setCurrentDirectory $ takeDirectory fn
            callCommand $ vhdlCheck ++ " " ++ fn -<.> "vhdl"
         ] else [])

      where extraFlags :: [String]
            extraFlags = if FlagV `elem` flags then ["-v"] else []

            vhdlCheck :: String
            vhdlCheck = fromMaybe "ghdl -s" $ msum $ flip map flags $ \ case
                  FlagVhdlChecker c -> Just $ sq c
                  _                 -> Nothing

            verilogCheck :: String
            verilogCheck = fromMaybe ("iverilog -Wall -g2012 " <> verilog) $ msum $ flip map flags $ \ case
                  FlagVerilogChecker c -> Just $ sq c
                  _                    -> Nothing

            verilog :: FilePath
            verilog = takeDirectory fn </> "verilog" </> "*.sv"

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
