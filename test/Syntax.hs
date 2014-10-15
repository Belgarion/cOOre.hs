module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Expr] [Expr]
  | Extern Name [Expr]
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

{-data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
-}
