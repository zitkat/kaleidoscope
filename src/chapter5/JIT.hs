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
            mainfn_n <- getFunctionN ee "main"
            case mainfn_n <|> mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()

          -- Return the optimized module
          return optmod

getFunctionN :: (ExecutionEngine e f) => ExecutableModule e -> String -> IO (Maybe f)
getFunctionN ee name = gFs >>= return . takeFs >>= return . mayLast
                    where -- gF :: Int -> IO (Maybe f) - TODO ScopedTypeVariables do not work in this case
                          gF i = EE.getFunction ee (fromString (gFN name i))
                          -- gFs :: IO [Maybe f]
                          gFs = mapM gF [1..1000]  -- TODO make this able to work with infinite list, some hints
                          takeFs fs = sequence $ takeWhile isJust fs
                          mayLast ml = ml >>= listToMaybe . reverse
                          


gFN :: String -> Int -> String
gFN name i = name ++ "." ++ show i