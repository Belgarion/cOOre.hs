module CodeGen where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace

import PrettyPrinting (join, ind)

typeToCtype :: Expr -> String
typeToCtype (Int _) = "int "
typeToCtype (String _) = "char * "
typeToCtype (Float _) = "float "

typestringToCtype :: Type -> String
typestringToCtype "hel" = "int "
typestringToCtype "sträng" = "char * "
typestringToCtype "flyt" = "float "
typestringToCtype "def" = "task "
typestringToCtype x = "aeurchaoeurchaoeurch (" ++ x ++ ")"

-- generates typdefs for c structs and c function args
-- 			 ast	    defs
genTypDef :: [Expr] -> String
genTypDef [] = ""
genTypDef ((BinaryOp "=" (Var name) typ):vars) = (typeToCtype typ) ++ name ++ ";\n" ++  (genTypDef vars)
--genTypDef ((Function t name params stmts):x) = (typestringToCtype t) ++ name ++ "(" ++ (join ", " [(typeToCtype typ) ++ name | (BinaryOp "=" (Var name) typ) <- params]) ++ ");\n" ++ (genTypDef x)
genTypDef (e) = "Other:: " ++ (show e)

-- checks if expr is an assignment
isAss :: Expr -> Bool
isAss (BinaryOp "=" _ _) = True
isAss _ = False

-- checks if expr is a function
isFunc :: Expr -> Bool
isFunc (Function _ _ _ _) = True
isFunc _ = False

-- generates klass-struct
--			klassname  ass 		struct
genStruct :: String -> [Expr] -> String
genStruct name stmts = "struct struct_" ++ name ++ " {\n" ++ (genTypDef [x|x <- stmts, isAss x]) ++ "};\nstruct struct_" ++ name ++ " " ++ name ++ ";\n" -- ++ (genTypDef [x | x <- stmts, isFunc x])

--			ast    klassnamn    indent  code
codeGen :: [Expr] -> String -> Int -> String
codeGen [] _ _ = ""
codeGen ((Klass name stmts):ast) _ depth = (ind depth) ++ genStruct name stmts ++ (codeGen stmts name depth) ++ "\n" ++ (codeGen ast "" depth)
codeGen ((BinaryOp name left right):ast) klass depth = (ind depth) ++ (codeGen [left] klass 0) ++ name ++ " " ++ (codeGen [right] klass 0) ++ ";\n" ++ (codeGen ast klass (depth))
codeGen ((Function t name params stmts):ast) klass depth = (ind depth) ++ (typestringToCtype t) ++ klass ++ "_" ++ name ++ "(" ++ (join ", " [(typeToCtype typ) ++ name | (BinaryOp "=" (Var name) typ) <- params]) ++ ") {\n" ++ (codeGen stmts klass (depth+1)) ++ "\n}\n" ++ (codeGen ast klass depth)
codeGen ((Var name):ast) klass depth = (ind depth) ++ klass ++ "." ++ name ++ " " ++ (codeGen ast klass depth)
codeGen ((Int value):ast) klass depth = (ind depth) ++ (show value) ++ (codeGen ast klass depth)
codeGen ((Float value):ast) klass depth = (ind depth) ++ (show value) ++ (codeGen ast klass depth)
codeGen ((String value):ast) klass depth = (ind depth) ++ (show value) ++ (codeGen ast klass depth)
codeGen ((If cond th el):ast) klass depth = (ind depth) ++ "if (" ++ (codeGen [cond] klass 0) ++ ") {\n" ++ (join "\n" [codeGen [x] klass (depth+1) | x <- th]) ++ "\n" ++ (ind depth) ++ "} else {\n" ++ (join "\n" [codeGen [x] klass (depth+1)| x <- el]) ++ "}\n" ++ (codeGen ast klass depth)
codeGen ((Return value):ast) klass depth = (ind depth) ++ "return " ++ (codeGen [value] klass 0) ++ ";\n" ++ (codeGen ast klass depth)
codeGen ((Call cklass name params):ast) klass depth = (ind depth) ++ cklass ++ "_" ++ name ++ "(" ++ (join ", " [codeGen [x] klass 0 | x <- params]) ++ ");\n" ++ (codeGen ast klass depth)
codeGen (expr:ast) klass depth = (ind depth) ++ "other :: " ++ (show expr) ++ "\n" ++ (codeGen ast klass (depth+1))


