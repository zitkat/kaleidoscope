{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
    debug,
  )
where

import qualified Codegen
import qualified JIT
import qualified Parser
import System.Directory
import System.Environment

import Control.Monad.Trans

import System.Console.Haskeline

import qualified LLVM.AST as AST
import Codegen


debug :: IO ()
debug = do
  contents <- readFile "src/chapter6/chapter6.k"
  let mast = Parser.parseToplevel contents
  case mast of
    Left err -> print err
    Right mod -> do
      lmod <- evalCodegen (codegenModule mod) 
      res <- JIT.runJIT lmod
      --print res
      putStrLn "Done."
      pure ()


repl :: IO ()
repl = runInputT defaultSettings (loop emptyCodegen)
  where
  loop codestate = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        let mast = Parser.parseToplevel input
        case mast of
          Left err -> outputStrLn (show err)
          Right curr -> do
            (lmod, codestate) <- liftIO $ runCodegen (codegenModule curr) codestate
            res <- liftIO $ JIT.runJIT lmod
            outputStrLn (show res)
            loop codestate


processFile :: FilePath -> IO ()
processFile fname = do
      exists <- doesFileExist fname
      if exists
        then do
          contents <- readFile fname
          let mast = Parser.parseToplevel contents
          case mast of
            Left err -> print err
            Right mod -> do
              lmod <- evalCodegen (codegenModule mod)
              res <- JIT.runJIT lmod
              print res
              putStrLn "Done."
        else putStrLn ("File " ++ fname ++ " not found.")


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    (fname:_) -> processFile fname
