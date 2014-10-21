module Syntax where

type Name = String

data Expr
  = Float Double
  | Int Integer
  | String String
  | Void
  | Var String
  | Call Name [Expr]
  | Function String Name [Expr] [Expr]
  | Return Expr
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
