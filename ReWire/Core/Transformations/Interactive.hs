{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Interactive environment for transforming ReWire Core programs.

module ReWire.Core.Transformations.Interactive (TransCommand,trans) where

import Prelude hiding (sequence,mapM)
import ReWire.Core.Syntax
import ReWire.Core.PrettyPrint (pp)
import ReWire.Core.PrettyPrintHaskell (ppHaskell)
import Unbound.LocallyNameless
import Text.Parsec (runParser,eof)
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find,intercalate)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import Data.Char
import ReWire.Core.Transformations.Expand (cmdExpand)
import ReWire.Core.Transformations.Reduce (cmdReduce)
import ReWire.Core.Transformations.Purge (cmdPurge)
import ReWire.Core.Transformations.Types
import System.IO

import Debug.Trace (trace)

-- Table of available commands.
type CommandTable = [(String,TransCommand)]

cmdPrint :: TransCommand
cmdPrint _ p = (Just p,Nothing)

cmdHelp :: TransCommand
cmdHelp _ _ = (Nothing,Just (intercalate ", " (map fst cmdTable)))

-- Here are the commands available.
cmdTable :: CommandTable
cmdTable = [(":p",cmdPrint),
            (":?",cmdHelp),
            ("expand",cmdExpand),
            ("reduce",cmdReduce),
            ("purge",cmdPurge)]

-- The "repl" for the translation environment.
trans :: RWCProg -> IO ()
trans p = do print (ppHaskell p)
             loop p
   where loop p = do putStr "> "
                     hFlush stdout
                     n <- getLine
                     let (cmd,n') = break isSpace n
                         args     = dropWhile isSpace n'
                     if cmd==":q" then return ()
                     else case lookup cmd cmdTable of
                               Just f  -> do let (mp,ms) = f args p
                                             case ms of
                                               Just s  -> putStrLn s
                                               Nothing -> return ()
                                             case mp of
                                               Just p' -> do print (ppHaskell p')
                                                             loop p'
                                               Nothing -> loop p
                               Nothing -> do if not (null n) then putStrLn $ "Invalid command: " ++ cmd else return ()
                                             loop p