--------------------------------------------------------------------
-- |
-- Module    :  Emit
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Emit where

import Data.String ( IsString(fromString) )
import Data.ByteString.Short ( ShortByteString )
import qualified Data.ByteString as BS
import LLVM.Module
import LLVM.Context
import LLVM.Analysis
import LLVM.PassManager

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.ExecutionEngine ( withMCJIT, withModuleInEngine, getFunction )

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT (runJIT)
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

toSig :: [ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double (fromString name) fnargs bls
  where
    fnargs = toSig (map fromString args)
    largs = map (\x -> (double, AST.Name (fromString x))) args
    bls = createBlocks $ execCodegen [] $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ args $ \a -> do
        var <- alloca double
        store var (local (AST.Name (fromString a)))
        assign (fromString a) var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double (fromString name) fnargs
  where fnargs = toSig (map fromString args)

codegenTop (S.BinaryDef name args body) =
  codegenTop $ S.Function ("binary" ++ name) args body

codegenTop (S.UnaryDef name args body) =
  codegenTop $ S.Function ("unary" ++ name) args body

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen [] $ do
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
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.Let name val body) = do
  i <- alloca double
  val <- cgen val
  store i val
  assign (fromString name) i  -- define variable `name`
  cgen body                   -- execute body of the let statement in context with variable `name` defined, possibly recursing into another let
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
    Nothing -> error ("No such operator " ++ show op)
cgen (S.Var x) = getvar (fromString x) >>= load
cgen (S.Int n) = return $ cons $ C.Float (F.Double (fromIntegral n))
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  let nargs = length largs in
    call (externf nargs (AST.Name (fromString fn))) largs
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  actifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  actifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi double [(trval, actifthen), (flval, actifelse)]

cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  ------------------
  i <- alloca double
  istart <- cgen start           -- Generate loop variable initial value
  stepval <- cgen step           -- Generate loop variable step

  store i istart                 -- Store the loop variable initial value
  assign (fromString ivar) i                  -- Assign loop variable to the variable name
  br forloop                     -- Branch to the loop body block

  -- for.loop
  ------------------
  setBlock forloop
  cgen body                      -- Generate the loop body
  ival <- load i                 -- Load the current loop iteration
  inext <- fadd ival stepval     -- Increment loop variable
  store i inext

  cond <- cgen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forloop forexit       -- Generate the loop jump

  -- for.exit
  ------------------
  setBlock forexit
  return zero

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen modo fns = do
  let modn = mapM codegenTop fns
      ast = runLLVM modo modn
  runJIT ast
  return ast
