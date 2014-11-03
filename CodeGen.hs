module CodeGen where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace

ind :: Int -> String
ind 0 = ""
ind x = (ind (x-1)) ++ "  "

typeToCtype :: Expr -> String
typeToCtype (Int _) = "int "
typeToCtype (String _) = "char * "
typeToCtype (Float _) = "float "

-- generates typdefs for c structs and c function args
-- 			 ast	    defs
genTypDef :: [Expr] -> String
genTypDef [] = ""
genTypDef ((BinaryOp "=" (Var name) typ):vars) = (typeToCtype typ) ++ name ++ ";" ++  (genTypDef vars)

-- checks if expr is an assinmanet
isAss :: Expr -> Bool
isAss (BinaryOp "=" _ _) = True
isAss _ = False 

-- generates klass-struct
--			klassname  ass 		struct
genStruct :: String -> [Expr] -> String
genStruct name stmts = "struct " ++ name ++ " { " ++ (genTypDef [x|x <- stmts, isAss x]) ++ "};\n"

--			ast    klassnamn  indent  code
codeGen :: [Expr] -> String -> Int -> String
codeGen [] _ _ = ""
codeGen ((Klass name stmts):ast) _ depth = genStruct name stmts ++ (codeGen stmts name depth) ++ "\n" ++ (codeGen ast name depth) 
codeGen (expr:ast) _ depth = (ind depth) ++ "other :: " ++ (show expr) ++ (codeGen ast "" depth)