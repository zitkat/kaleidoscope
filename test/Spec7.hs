module Main where

import Parser ( parseToplevel )
import Emit ( codegen )
import Codegen ( emptyModule ) 
import Syntax
import qualified LLVM.AST as AST
import Test.Hspec
import SpecCommon ( cspec )


main :: IO ()
main = hspec spec 

spec = do describe "common" cspec