module Syntax where

type Name = String
type Type = String

data Expr
  = Float Double
  | Int Integer
  | String String
  | Void
  | Var String
  | Call Name Name [Expr]
  | Function Type Name [Expr] [Expr]
  | Return Expr
  | Claim Name [Expr]
  | Extern Name [Expr]
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | Klass Name [Expr]
  | If Expr [Expr] [Expr]
  | For Expr Expr Expr [Expr]
  | Async Expr Expr Expr
  deriving (Eq, Ord, Show)

{-data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
-}
