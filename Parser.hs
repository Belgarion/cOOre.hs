module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Control.Monad.Trans
import System.IO
import Text.Parsec.Prim

-- inflix fmap
import Control.Applicative ((<$>))
import Lexer
import Syntax

import Control.Monad.Trans

import System.IO
import System.IO.Unsafe

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
  return $ Int n

floating :: Parser Expr
floating = do
  n <- float
  return $ Float n

strings :: Parser Expr
strings = do
  s <- str
  return $ String s


expr :: Parser Expr
expr = try exprnum
    <|> exprctlr

exprnum :: Parser Expr
exprnum = Ex.buildExpressionParser (binops) factor

exprctlr :: Parser Expr
exprctlr = _if <|> for <|> _return <|> claim

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

function :: Parser Expr
function = do
  t <- choice [reservedReturn "def",reservedReturn "hel",reservedReturn "flyt",reservedReturn "sträng"]
  name <- identifier
  args <- parens $ many arg
  body <- many expr
  reserved "klar"
  return $ Function t name args body

arg :: Parser Expr
arg = do 
  var <- variable
  reservedOp "="
  typ <- types
  return $ BinaryOp "=" var typ
--ret <- option (Void) (do{reserved "återvänd"; d<-expr; return d})

reservedReturn :: String -> Parser String
reservedReturn x = do
  reserved x
  return x

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = try callInside <|> callOutside

callInside :: Parser Expr
callInside = do
  name <- identifier
  args <- parens $ many expr
  return $ Call "" name args

callOutside :: Parser Expr
callOutside = do
  struct <-identifier;
  reservedOp "§"
  name <- identifier
  args <- parens $ many expr
  return $ Call struct name args


factor :: Parser Expr
factor = types
      <|> try extern
      <|> try function
      <|> try call
      <|> try async
      <|> variable
      <|> parens expr

types :: Parser Expr
types =  try floating
      <|> try int
      <|> try strings

claim :: Parser Expr
claim = do
    reserved "begär"
    name <- identifier
    stmts <- many expr
    reserved "klar"
    return $ Claim name stmts


_return :: Parser Expr
_return = do
    reserved "återvänd"
    value <- option (Void) (do{ d<-expr; return d})
    return $ Return value

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

async :: Parser Expr
async = do
    reserved "async"
    after <- option (Int 0) (do{reserved "efter"; d<-int; return d})
    before <- option (Int 0) (do{reserved "före"; d<-int; return d})
    body <- call
    return $ Async before after body

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
    def <- klass <|> include
    return def

klass :: Parser Expr
klass = do
    reserved "struktur"
    name <- identifier
    stmts <- many defn
    reserved "meep"
    return $ Klass name stmts

include :: Parser Expr
include = do
    reserved "referera"
    name <- identifier
    let ast = unsafePerformIO $ fileToAst name
    case ast of
      Left _ -> return $ Include name []
      Right x -> return $ Include name x

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

fileToAst :: String -> IO(Either ParseError [Expr])
fileToAst fname = do
    filec <- readFile fname
    return $ parseToplevel filec

