module Main where

import Parser ( parseToplevel )
import Emit ( codegen )
import Codegen ( emptyModule )
import Data.String ( IsString(fromString) )
import Control.Monad.Trans ( MonadIO(liftIO) )
import qualified Control.Monad as M

import System.Environment ( getArgs )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT )

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
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> M.void (processFile fname)
    _ -> print "Only one argument <file> can be provided."
