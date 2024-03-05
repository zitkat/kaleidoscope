module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

binops = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

int :: Parser Expr
int = Float . fromInteger <$> integer

floating :: Parser Expr
floating = Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function =  do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  Function name args <$> expr

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
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try extern   -- there can be external declaration inside expresion factors?
      <|> try function -- def, there can be function definitions inside expressions?
      <|> try call
      <|> variable
      <|> parens expr -- expression can be a factor but only in parens

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer -- skip white space at the start
  r <- p               -- parse whatewer p parses
  eof                  -- until EOF
  return r             -- return what parsed

toplevel :: Parser [Expr]
toplevel = many $ do   -- on top level there are many
    def <- defn        -- but only functions, extern functions or expressions
    reservedOp ";"     -- an definitions end with semicolon 
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
