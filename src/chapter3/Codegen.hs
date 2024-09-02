{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Codegen
  ( Codegen,
    runCodegen,
    evalCodegen,
    codegenModule,
    CodegenState,
    emptyCodegen
  )
where

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short
    ( ShortByteString, fromShort, toShort )
import qualified Data.Map as Map
import Data.String
import qualified Data.Text.Lazy.IO as T
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as AST
import LLVM.IRBuilder as IRB
import qualified LLVM.PassManager as LLPM
import LLVM.Pretty
import Syntax
import Text.Pretty.Simple
import LLVM.AST (Name)

-------------------------------------------------------------------------------
-- Code Generator Monad
-------------------------------------------------------------------------------

type Codegen = (StateT CodegenState IO)

data CodegenState
  = CodegenState
      { symbolTable :: Map.Map Name AST.Operand,
        functionTable :: Map.Map Name AST.Operand,
        modDefinitions :: [AST.Definition],
        nameSupply :: Word
      }

evalCodegen :: Codegen a -> IO a
evalCodegen = flip evalStateT emptyCodegen

runCodegen :: StateT CodegenState m a -> CodegenState -> m (a, CodegenState)
runCodegen = runStateT

emptyCodegen :: CodegenState
emptyCodegen = CodegenState
  { symbolTable = Map.empty,
    functionTable = Map.empty,
    modDefinitions = [],
    nameSupply = 0
  }

-- Everything is a double!
doubleTy :: AST.Type
doubleTy = AST.FloatingPointType AST.DoubleFP

printdTy :: [AST.Type] -> AST.Type
printdTy argtys = AST.PointerType (AST.FunctionType doubleTy argtys False) (AST.AddrSpace 0)

-------------------------------------------------------------------------------
-- Scoping
-------------------------------------------------------------------------------

getvar :: Name -> ModuleBuilderT Codegen AST.Operand
getvar name = do
  res <- Map.lookup name <$> gets symbolTable
  case res of
    Just x -> pure x
    Nothing -> error ("unknown variable: " ++ show name)

getfun :: Name -> [AST.Type] -> ModuleBuilderT Codegen AST.Operand
getfun name tys = do
  res <- Map.lookup name <$> gets symbolTable
  case res of
    Just x -> pure x
    Nothing -> pure (AST.ConstantOperand (C.GlobalReference (printdTy tys) name))

assignvar :: Name -> AST.Operand -> ModuleBuilderT Codegen ()
assignvar name var = modify (\s -> s {symbolTable = Map.insert name var (symbolTable s)})

-------------------------------------------------------------------------------
-- Code Generator
-------------------------------------------------------------------------------

codegen :: Expr -> IRBuilderT (ModuleBuilderT Codegen) AST.Operand
codegen = \case
  Float d -> pure (double d)
  Var name -> (lift $ getvar name) >>= flip load 0
  UnaryOp op e -> do
    codegen (Call (prefixName "unary" op) [e])
  BinaryOp "=" (Var var) val -> do
    i <- lift (getvar var)
    v <- codegen val
    store i 0 v
    return v
  BinaryOp op l r -> do
    let callWithOps f = join (f <$> codegen l <*> codegen r)
    case op of
      "+" -> callWithOps fadd
      "-" -> callWithOps fsub
      "*" -> callWithOps fmul
      "<" -> callWithOps (fcmp AST.ULT) >>= (\operand -> uitofp operand doubleTy)
      _ -> codegen (Call (prefixName "binary" op) [l, r])
  Call name args -> do
    largs <- traverse (fmap (,[]) . codegen) args
    lfun <- lift (getfun name (replicate (Prelude.length largs) doubleTy))
    call lfun largs

codegenDefn :: Expr -> (ModuleBuilderT Codegen) AST.Operand
codegenDefn = \case
  Function name args body -> do
    funcOperand <- function
      name
      [(doubleTy, ParameterName (unpackName nm)) | nm <- args]
      doubleTy
      $ \argOs -> do
        entryBlock <- block `named` "entry"
        forM_ (zip args argOs) $ \(name, arg) -> do
          a <- alloca doubleTy Nothing 0
          store a 0 arg
          lift $ assignvar name a
        retval <- codegen body
        ret retval
    modify (\s -> s {functionTable = Map.insert name funcOperand (functionTable s)})
    return funcOperand

-------------------------------------------------------------------------------
-- Name Handling
-------------------------------------------------------------------------------

prefixName :: String -> Name -> Name
prefixName pre (AST.Name nm) = AST.mkName (pre <> unpackBS nm)
prefixName pre (AST.UnName nm) = AST.mkName (pre <> show nm)

unpackBS :: ShortByteString -> String
unpackBS x = BS.unpack (fromShort x)

packShort :: String -> ShortByteString
packShort = toShort . BS.pack

unpackName :: AST.Name -> ShortByteString
unpackName (AST.Name nm) = nm

getLastAnon :: Codegen (Maybe String)
getLastAnon = do
  lastAnonId <- gets nameSupply
  return (if lastAnonId == 0 then Nothing else Just ("anon" ++ show lastAnonId))

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

passes :: LLPM.PassSetSpec
passes = LLPM.defaultCuratedPassSetSpec {LLPM.optLevel = Just 3}

codegenModule :: [Expr] -> Codegen AST.Module
codegenModule phrases = do
  modDefs <- gets modDefinitions
  anonLabeledPhrases <- traverse return phrases
  --liftIO (pPrint anonLabeledPhrases)
  defs <- IRB.execModuleBuilderT IRB.emptyModuleBuilder (mapM_ codegenDefn anonLabeledPhrases)
  let updatedDefs = modDefs ++ defs
  modify (\s -> s {modDefinitions = updatedDefs})
  mod <- IRB.buildModuleT (packShort "<stdin>") (traverse IRB.emitDefn updatedDefs)
  liftIO (T.putStrLn (ppllvm mod))
  pure mod
