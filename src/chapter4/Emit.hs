{-# LANGUAGE OverloadedStrings #-}

module Emit where

import Data.String ( IsString(fromString) )
import Data.ByteString.Short ( ShortByteString )
import qualified Data.ByteString as BS

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Control.Monad.Except ( forM )
import qualified Data.Map as Map

import Codegen
    ( addBlock,
      alloca,
      assign,
      call,
      cons,
      createBlocks,
      define,
      double,
      entryBlockName,
      execCodegen,
      external,
      externf,
      fadd,
      fcmp,
      fdiv,
      fmul,
      fsub,
      getvar,
      load,
      local,
      ret,
      runLLVM,
      setBlock,
      store,
      uitofp,
      Codegen,
      LLVM )
import JIT ( runJIT )
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
  let nargs = length largs in
    call (externf nargs (AST.Name (fromString fn)) ) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  runJIT oldast
  where
    modn    = mapM codegenTop fns
    oldast  = runLLVM mod modn
