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
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory, takeFileName)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (callCommand)
import Test.Tasty (defaultMain, sequentialTestGroup, TestTree, DependencyType (..))
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Golden (goldenVsFileDiff)

import Paths_rewire (getDataFileName)

data Flag = FlagH
          | FlagNoCheck
          | FlagNoDTypes
          | FlagNoGhc
          | FlagV
          | FlagVerilogChecker String
          | FlagVhdl
          | FlagVhdlChecker String
          -- Tasty options.
          | FlagP String
          | FlagQ
          | FlagL
          | FlagT String
          | FlagColor String
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
       [ Option ['h'] ["help"]            (NoArg FlagH)                         "Show this help text."
       , Option ['v'] ["verbose"]         (NoArg FlagV)                         "More verbose output."
       , Option []    ["vhdl"]            (NoArg FlagVhdl)                      "Test VHDL code generation in addition to Verilog."
       , Option []    ["no-check"]        (NoArg FlagNoCheck)                   "Disable verification of output HDL with checker."
       , Option []    ["no-ghc"]          (NoArg FlagNoGhc)                     "Disable running tests through ghc."
       , Option []    ["vhdl-checker"]    (ReqArg FlagVhdlChecker "command")    "Set the command to use for checking generated VHDL (default: 'ghdl -s')."
       , Option []    ["verilog-checker"] (ReqArg FlagVerilogChecker "command") "Set the command to use for checking generated Verilog (default: 'iverilog -Wall -g2012')."

       -- Tasty arguments.
       , Option ['p'] ["pattern"]         (ReqArg FlagP "PATTERN")               "Select only tests which satisfy a pattern or awk expression."
       , Option ['q'] ["quiet"]           (NoArg FlagQ)                          "Do not produce any output; indicate success only by the exit code."
       , Option ['l'] ["list-tests"]      (NoArg FlagL)                          "Do not run the tests; just print their names."
       , Option ['t'] ["timeout"]         (ReqArg FlagT "DURATION")              "Timeout for individual tests (suffixes: ms, s, m, h; default: s)."
       , Option []    ["color"]           (ReqArg FlagColor "never|always|auto") "When to use colored output (default: auto)."
       ]

testCompiler :: [Flag] -> FilePath -> [TestTree]
testCompiler flags fn =
      -- Test: compile Haskell source with GHC
      (if FlagNoGhc `elem` flags then []
      else [ testCase (takeBaseName fn <> " (stack ghc)") $ do
            cdTestdir
            callCommand $ "stack ghc " <> fn
      ])

      --- Verilog tests ---

      -- Test: compile Haskell to Verilog with RWC.
      <> [ golden "sv" $ do
            cdTestdir
            withArgs (fn : ["-o", ofile "sv"] <> extraFlags) RWC.main
         ]

      -- Test: check Verilog output.
      <> (if FlagNoCheck `elem` flags then []
         else [ testCase (takeBaseName fn <> " (" <> verilogChecker <> ")") $ do
            cdTestdir
            callCommand $ verilogCheck <> " " <> ofile "sv"
         ])

      --- VHDL tests ---

      -- Test: compile Haskell to VHDL with RWC.
      <> (if FlagVhdl `elem` flags
         then [ testCase (takeBaseName fn <> " (VHDL output)") $ do
            cdTestdir
            withArgs ("--vhdl" : fn : ["-o ", ofile "vhdl"] <> extraFlags) RWC.main
         ] else [])

      -- Test: check VHDL output.
      <> (if FlagVhdl `elem` flags && FlagNoCheck `notElem` flags
         then [ testCase (takeBaseName fn <> " (" <> vhdlChecker <> ")") $ do
            cdTestdir
            callCommand $ vhdlCheck <> " " <> ofile "vhdl"
         ] else [])

      where extraFlags :: [String]
            extraFlags = if FlagV `elem` flags then ["-v"] else []

            cdTestdir :: IO ()
            cdTestdir = setCurrentDirectory $ takeDirectory fn

            ofile :: String -> FilePath
            ofile ext = fn -<.> ("out." <> ext)

            vhdlChecker :: String
            vhdlChecker = "ghdl"

            vhdlCheck :: String
            vhdlCheck = fromMaybe (vhdlChecker <> " -s ") $ msum $ flip map flags $ \ case
                  FlagVhdlChecker c -> Just $ sq c
                  _                 -> Nothing

            verilogChecker :: String
            verilogChecker = "iverilog"

            verilogCheck :: String
            verilogCheck = fromMaybe (verilogChecker <> " -Wall -g2012 " <> verilog) $ msum $ flip map flags $ \ case
                  FlagVerilogChecker c -> Just $ sq c
                  _                    -> Nothing

            verilog :: FilePath
            verilog = takeDirectory fn </> "verilog" </> "*.sv"

            golden :: FilePath -> IO () -> TestTree
            golden ext = goldenVsFileDiff (takeBaseName fn <> " (golden verilog)") diff gold out
                  where gold = fn -<.> ext
                        out  = ofile ext

            diff :: FilePath -> FilePath -> [String]
            diff ref new = ["diff", "-bu", ref, new]

sq :: String -> String
sq = \ case
      '"'  : s | last s == '"'  -> init s
      '\'' : s | last s == '\'' -> init s
      s                         -> s

getTests :: [Flag] -> FilePath -> IO TestTree
getTests flags dirName = do
      dir   <- getDataFileName ("tests" </> dirName)
      files <- map (dir </>) . filter (".hs" `isSuffixOf`) <$> listDirectory dir
      pure $ sequentialTestGroup dirName AllFinish $ concatMap (testCompiler flags) files

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc-test [OPTION...]" options) >> exitFailure

main :: IO ()
main = do
      (flags, extra, errs) <- getOpt Permute options <$> getArgs

      let testDirs = ["regression"] -- TODO(chathhorn): re-enable integration tests.

      unless (null errs && FlagH `notElem` flags) $ do
            mapM_ (hPutStrLn stderr) errs
            exitUsage

      tests <- mapM (getTests flags) testDirs

      withArgs (injectTastyArgs flags) $ defaultMain $ sequentialTestGroup "Tests" AllFinish tests

      where injectTastyArgs :: [Flag] -> [String]
            injectTastyArgs = concatMap toTastyArg

            toTastyArg :: Flag -> [String]
            toTastyArg = \ case
                  FlagP p     -> ["-p", p]
                  FlagQ       -> ["-q"]
                  FlagL       -> ["-l"]
                  FlagT t     -> ["-t", t]
                  FlagColor c -> ["--color", c]
                  _           -> []
