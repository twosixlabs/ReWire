{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
module ReWire.Flags where

import Data.Hashable (Hashable)
import GHC.Generics (Generic (..))

data Flag = FlagO !String
          | FlagVerbose | FlagHelp
          | FlagFirrtl  | FlagVerilog | FlagVhdl | FlagCore | FlagFromCore
          | FlagDump !String
          | FlagFlatten
          | FlagInvertReset
          | FlagNoReset | FlagNoClock
          | FlagSyncReset
          | FlagVhdlPkgs !String
          | FlagClockName !String
          | FlagResetName !String
          | FlagInputNames !String | FlagOutputNames !String | FlagStateNames !String
          | FlagLoadPath !String
          | FlagStart !String
          | FlagTop !String
          | FlagInterpret !(Maybe String) | FlagCycles !String
          | FlagEvalDepth !String
          | FlagPretty
          | FlagDebugTypeCheck
          | FlagRtlOpt !String
      deriving (Eq, Show, Generic)

instance Hashable Flag
