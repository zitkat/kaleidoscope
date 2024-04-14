--------------------------------------------------------------------
-- |
-- Module    :  Main
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Parser ( parseToplevel )
import Codegen ( emptyModule )
import Emit ( codegen )

import Control.Monad.Trans ( MonadTrans(lift) )

import System.Environment ( getArgs )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )

import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop :: AST.Module -> InputT IO ()
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- lift $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    (fname:_) -> processFile fname >> return ()
