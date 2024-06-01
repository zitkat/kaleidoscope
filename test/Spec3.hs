import Parser ( parseToplevel )
import Emit ( codegen )
import Codegen ( emptyModule ) 
import qualified LLVM.AST as AST

main :: IO ()
main = process mod "1 + 1;" >> return ()
    where 
        mod = emptyModule "my cool test"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast
