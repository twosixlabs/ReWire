{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module Embedder.Config
      ( interpret, Config, getOutFile, getEmbedFile, getAtmoFile
      , Language (..), ResetFlag (..), OutFlag (..)
      , verbose, pretty, flatten
      , target, outFlags, inputsFile, outFile
      , start, top, loadPath, dump, source
      ) where

import Embedder.Flags (Flag (..))

import Control.Lens (makeLenses, over, (.~), (^.), Lens', lens)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text, pack, unpack, splitOn)
import Numeric.Natural (Natural)
import System.FilePath ((-<.>))

import qualified Data.Set as Set

data Language = Interpret | FIRRTL | VHDL | Verilog | RWCore | Haskell | Isabelle
      deriving (Eq, Ord, Show)
data ResetFlag = Inverted | Synchronous
      deriving (Eq, Ord, Show)
data OutFlag   = Flatten | Pretty | Verbose
      deriving (Eq, Ord, Show)

data Config = Config
      { _source       :: Language
      , _target       :: Language
      , _outFlags     :: Set OutFlag
      , _inputsFile   :: FilePath
      , _outFile      :: Maybe FilePath
      , _start        :: Text
      , _top          :: Text
      , _loadPath     :: [FilePath]
      , _dump         :: Natural -> Bool
      }

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
      { _source       = Haskell
      , _target       = Verilog
      , _outFlags     = mempty
      , _inputsFile   = "inputs.yaml"
      , _outFile      = Nothing
      , _start        = "Main.start"
      , _top          = "top_level"
      , _loadPath     = []
      , _dump         = const False
      }

verbose :: Lens' Config Bool
verbose = lens (getOutFlag Verbose) (setOutFlag Verbose)



pretty :: Lens' Config Bool
pretty = lens (getOutFlag Pretty) (setOutFlag Pretty)

flatten :: Lens' Config Bool
flatten = lens (getOutFlag Flatten) (setOutFlag Flatten)

getOutFlag :: OutFlag -> Config -> Bool
getOutFlag f conf = f `Set.member` (conf^.outFlags)

setOutFlag :: OutFlag -> Config -> Bool -> Config
setOutFlag f conf True  = over outFlags (Set.insert f) conf
setOutFlag f conf False = over outFlags (Set.delete f) conf

type ErrorMsg = Text

getOutFile :: Config -> FilePath -> FilePath
getOutFile c filename = flip fromMaybe (c^.outFile) $ case c^.target of
      Verilog   -> filename -<.> "v"
      FIRRTL    -> filename -<.> "fir"
      VHDL      -> filename -<.> "vhdl"
      Interpret -> filename -<.> "yaml"
      RWCore    -> filename -<.> "rwc"
      Haskell   -> filename -<.> "hs"
      Isabelle  -> filename -<.> "thy"

getEmbedFile :: Config -> FilePath -> FilePath
getEmbedFile c filename = flip fromMaybe (c^.outFile) $ filename -<.> "thy"

getAtmoFile :: Config -> FilePath -> FilePath
getAtmoFile c filename = flip fromMaybe (c^.outFile) $ filename -<.> "atmo"

-- TODO(chathhorn): separate validation pass.
interpret :: [Flag] -> Either ErrorMsg Config
interpret = foldM interp defaultConfig
      where interp :: Config -> Flag -> Either ErrorMsg Config
            interp c = \ case
                  FlagHelp                        -> Left ""
                  FlagLoadPath (pack -> p)        -> pure $ over loadPath (<> map unpack (splitOn' "," p)) c
                  FlagO p | Nothing <- c^.outFile -> pure $ outFile .~ pure p   $ c
                          | otherwise             -> Left "Multiple output files specified on the command line."
                  FlagFlatten                     -> pure $ over outFlags (Set.insert Flatten) c
                  FlagPretty                      -> pure $ over outFlags (Set.insert Pretty) c
                  FlagVerbose                     -> pure $ over outFlags (Set.insert Verbose) c
                  FlagDump (pack -> d)            -> pure $ over dump (augment $ map (read . unpack) $ splitOn' "," d) c
                  FlagStart (pack -> n)           -> pure $ start .~ n $ c
                  FlagTop (pack -> n)             -> pure $ top .~ n $ c
                  
            augment :: [Natural] -> (Natural -> Bool) -> Natural -> Bool
            augment ns f n | n `elem` ns = True
                           | otherwise   = f n

            -- | Version of splitOn that returns '[]' instead of '[""]' when the second argument is empty.
            splitOn' :: Text -> Text -> [Text]
            splitOn' sep = \ case
                  "" -> []
                  s  -> splitOn sep s
