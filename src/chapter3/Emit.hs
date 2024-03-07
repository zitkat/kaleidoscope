{-# LANGUAGE OverloadedStrings #-}

module Emit where

import Data.String ( IsString(fromString) )
import Data.ByteString.Short ( ShortByteString )
import qualified Data.ByteString as BS

import LLVM.Module ( moduleLLVMAssembly, withModuleFromAST )
import LLVM.Context ( withContext )

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Control.Monad.Except ( forM )
import qualified Data.Map as Map

import Codegen
    ( Codegen,
      LLVM,
      runLLVM,
      define,
      external,
      double,
      createBlocks,
      entryBlockName,
      execCodegen,
      addBlock,
      setBlock,
      assign,
      getvar,
      local,
      externf,
      fadd,
      fsub,
      fmul,
      fdiv,
      fcmp,
      cons,
      uitofp,
      call,
      alloca,
      store,
      load,
      ret )
import qualified Syntax as S

toSig :: [ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double (fromString name) fnargs bls
  where
    fnargs = toSig (map fromString args)
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name (fromString a)))
        assign (fromString a) var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double (fromString name) fnargs
  where fnargs = toSig (map fromString args)

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call (fromString ("unary" ++ show op)) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar (fromString var)
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar (fromString x) >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name (fromString fn))) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    BS.putStr llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
