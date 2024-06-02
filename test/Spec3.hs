module Main where

import Parser ( parseToplevel )
import Emit ( codegen )
import Codegen ( emptyModule ) 
import Syntax
import qualified LLVM.AST as AST

import Test.Hspec


main :: IO ()
main = hspec spec 

spec = do 
  it "def foo(a b) a*a + 2*a*b + b*b;" $ parseToplevel "def foo(a b) a*a + 2*a*b + b*b;" `shouldBe` Right [Function "foo" ["a","b"] 
                                                                                                                (BinaryOp "+" 
                                                                                                                  (BinaryOp "+" 
                                                                                                                    (BinaryOp "*" (Var "a") (Var "a")) 
                                                                                                                    (BinaryOp "*"  
                                                                                                                      (BinaryOp "*" (Float 2.0) (Var "a")) 
                                                                                                                      (Var "b"))) 
                                                                                                                  (BinaryOp "*" (Var "b") (Var "b")))]