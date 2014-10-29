module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-","§"]
    names = ["def","extern","binary", "unary", "meep", "klar", "om", "annars", "för", "async", "efter", "före", "hel","flyt","sträng","återvänd","begär", "referera"]
    style = emptyDef {
               Tok.commentLine = "°"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

str :: Parser String
str = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whitespace = Tok.whiteSpace lexer

operator :: Parser String
operator = do
	c <- Tok.opStart emptyDef
	cs <- many $ Tok.opLetter emptyDef
	return (c:cs)
