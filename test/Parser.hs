module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

-- inflix fmap
import Control.Applicative ((<$>))
import Lexer
import Syntax

--binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
--unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [[binary "=" Ex.AssocLeft]
         ,[binary "*" Ex.AssocLeft,
           binary "/" Ex.AssocLeft]
         ,[binary "+" Ex.AssocLeft,
           binary "-" Ex.AssocLeft]
         ,[binary "<" Ex.AssocLeft]]

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = do
  n <- float
  return $ Float n

expr :: Parser Expr
expr = try exprnum
    <|> exprctlr

exprnum :: Parser Expr
exprnum = Ex.buildExpressionParser (binops) factor

exprctlr :: Parser Expr
exprctlr = _if <|> for

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- many expr
  reserved "klar"
  return $ Function name args body

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
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr

_if :: Parser Expr
_if = do
    reserved "om"
    cond <- expr
    tbod <- many expr
    ebod <- option [] _else
    reserved "klar"
    return $ If cond tbod ebod

_else :: Parser [Expr]
_else = do
    reserved "annars"
    ebod <- many expr
    return ebod

for :: Parser Expr
for = do
    reserved "för"
    before <- expr
    cond <- expr
    after <- expr
    body <- many expr
    reserved "klar"
    return $ For before cond after body

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
    def <- klass
    return def

klass :: Parser Expr
klass = do
    reserved "struktur"
    name <- identifier
    stmts <- many defn
    reserved "meep"
    return $ Klass name stmts

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
