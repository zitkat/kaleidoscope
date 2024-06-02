module SpecCommon where

import Parser ( parseToplevel )
import Emit ( codegen )
import Codegen ( emptyModule ) 
import Syntax
import qualified LLVM.AST as AST
import Test.Hspec


cspec:: Spec
cspec = do
  it "parse 1 + 1;" $ do parseToplevel "1 + 1;" `shouldBe` Right [BinaryOp "+" (Int 1) (Int 1)]
