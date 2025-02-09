module JIT where

import Foreign.Ptr ( FunPtr, castFunPtr )
import Data.String ( IsString(fromString) )
import qualified Data.ByteString as BS

import LLVM.Context ( withContext, Context )
import LLVM.Module as Mod
    ( moduleAST, moduleLLVMAssembly, withModuleFromAST )
import qualified LLVM.AST as AST

import LLVM.PassManager
    ( defaultCuratedPassSetSpec,
      runPassManager,
      withPassManager,
      PassSetSpec(optLevel) )

import LLVM.Analysis ( verify )

import qualified LLVM.ExecutionEngine as EE
import LLVM.Internal.Analysis (verify)

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
            mainfn <- EE.getFunction ee (AST.Name $ fromString "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()

          -- Return the optimized module
          return optmod
