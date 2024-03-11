{-# LANGUAGE ScopedTypeVariables #-}

module JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )
import Data.String ( IsString(fromString) )
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Except
import Control.Applicative

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE
import LLVM.Internal.Analysis (verify)
import GHC.Generics (Generic1(from1))
import LLVM.Internal.ExecutionEngine (ExecutableModule, ExecutionEngine)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO AST.Module
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          succ <- runPassManager pm m
          verify m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          BS.putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (fromString "main")
            mainfn_n <- getLastFunctionN ee "main"
            case mainfn_n <|> mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()
          -- Return the optimized module
          return optmod


getLastFunctionN :: (ExecutionEngine e f) => ExecutableModule e -> String -> IO (Maybe f)
getLastFunctionN ee name = do
  fs <- gatherFunctions ee name
  case fs of
    x:_ -> return $ Just x
    [] -> return Nothing

genFName :: String -> Int -> String
genFName name i = name ++ "." ++ show i

gatherFunctions :: (ExecutionEngine e f) => ExecutableModule e -> String -> IO [f]
gatherFunctions ee name = gatherFunctions' 1 []
  where 
    -- gF :: Int -> IO (Maybe f)
    gF i = EE.getFunction ee (fromString (genFName name i))
    -- gFs :: IO [f]
    gatherFunctions' i fs = do
              fun <- gF i
              case fun of
                Nothing -> return fs
                Just fn -> gatherFunctions' (i+1) (fn : fs)
