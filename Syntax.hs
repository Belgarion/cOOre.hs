module Syntax where

import Data.Map

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
  | Include Name [Expr]
  | IncludeCore Name [Expr]
  | If Expr [Expr] [Expr]
  | For Expr Expr Expr [Expr]
  | Async Expr Expr Expr
  deriving (Eq, Ord, Show)

type FunctionsMap = Data.Map.Map String String
type VariablesMap = Data.Map.Map String String
type FancyAST = [(VariablesMap, FancyExpr)]
type FancyASTEntry = (VariablesMap, FancyExpr)
data FancyExpr -- for FancyAst
  = FloatF Double
  | IntF Integer
  | StringF String
  | VoidF
  | VarF String
  | CallF Name Name FancyAST
  | FunctionF Type Name [Expr] FancyAST
  | ReturnF FancyASTEntry
  | ClaimF Name FancyAST
  | ExternF Name FancyAST
  | BinaryDefF Name [Name] FancyExpr
  | UnaryDefF Name [Name] FancyExpr
  | BinaryOpF Name FancyASTEntry FancyASTEntry
  | UnaryOpF Name FancyExpr
  | KlassF Name FancyAST
  | IncludeF Name FancyAST
  | IncludeCoreF Name FancyAST
  | IfF Expr FancyAST FancyAST
  | ForF FancyASTEntry FancyASTEntry FancyASTEntry FancyAST
  | AsyncF FancyExpr FancyExpr FancyExpr
  deriving (Eq, Ord, Show)
