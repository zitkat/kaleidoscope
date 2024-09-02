module Parser where

import Text.Parsec ( ParseError, (<|>), many, parse, try, eof )
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
    ( commaSep,
      float,
      identifier,
      integer,
      lexer,
      parens,
      reserved,
      reservedOp )
import Syntax
    ( Expr(Call, Float, BinaryOp, Var, Function, Extern) )
import LLVM.AST (Name, mkName)

int :: Parser Expr
int = Float . fromInteger <$> integer

floating :: Parser Expr
floating = Float <$> float

name :: Parser Name
name = mkName <$> identifier

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp (mkName s))) assoc

binops = [[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]]

expr :: Parser Expr
expr = Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = Var <$> name

function :: Parser Expr
function =  do
  reserved "def"
  nm <- name
  args <- parens $ many name 
  Function nm args <$> expr

-- originally 
  -- do
  -- reserved "def"
  -- name <- identifier
  -- args <- parens $ many variable
  -- body <- expr
  -- return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  nm <- name
  args <- parens $ many name 
  return $ Extern nm args

call :: Parser Expr
call = do
  nm <- name
  args <- parens $ commaSep expr
  return $ Call nm args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> parens expr

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
