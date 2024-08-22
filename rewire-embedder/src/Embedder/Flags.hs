{-# LANGUAGE Safe #-}
module Embedder.Flags where

data Flag = FlagO !String
          | FlagVerbose | FlagHelp
          | FlagDump !String
          | FlagFlatten
          | FlagLoadPath !String
          | FlagStart !String
          | FlagTop !String
          | FlagPretty
      deriving (Eq, Show)
