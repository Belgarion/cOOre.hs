module CodeGen where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace

import PrettyPrinting (join, ind)

inEnv env (BinaryOpF "=" (envl, (VarF name)) (renv, right)) = case (Data.Map.lookup name env) of
    Just _ -> True
    Nothing -> False
inEnv env other = trace (show other) False

typeToCtype :: Expr -> String
typeToCtype (Int _) = "int "
typeToCtype (String _) = "char * "
typeToCtype (Float _) = "float "

typeToCtype' :: FancyExpr -> String
typeToCtype' (IntF _) = "int "
typeToCtype' (StringF _) = "char * "
typeToCtype' (FloatF _) = "float "

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

genTypDef' :: [FancyExpr] -> String
genTypDef' [] = ""
genTypDef' ((BinaryOpF "=" (env, (VarF name)) (envr, typ)):vars) = (typeToCtype' typ) ++ name ++ ";\n" ++  (genTypDef' vars)
--genTypDef ((Function t name params stmts):x) = (typestringToCtype t) ++ name ++ "(" ++ (join ", " [(typeToCtype typ) ++ name | (BinaryOp "=" (Var name) typ) <- params]) ++ ");\n" ++ (genTypDef x)
genTypDef' (e) = "Other:: " ++ (show e)

genTypDefSingle :: VariablesMap -> FancyExpr -> String
genTypDefSingle env (BinaryOpF "=" (e,(VarF name)) (e2,typ)) =
    typedef ++ name ++ " = " ++ (fancyCodeGen Data.Map.empty [(e2,typ)]  "" 0)
    where
        typedef = case (Data.Map.lookup name env) of
            Just x -> ""
            Nothing -> (typeToCtype' typ)


-- checks if expr is an assignment
isAss :: Expr -> Bool
isAss (BinaryOp "=" _ _) = True
isAss _ = False

isAss' :: FancyASTEntry -> Bool
isAss' (_, (BinaryOpF "=" _ _)) = True
isAss' _ = False

-- checks if expr is a function
isFunc :: Expr -> Bool
isFunc (Function _ _ _ _) = True
isFunc _ = False

-- generates klass-struct
--			klassname  ass 		struct
genStruct :: String -> [Expr] -> String
genStruct name stmts = "struct struct_" ++ name ++
    " {\n" ++ (genTypDef [x|x <- stmts, isAss x]) ++ "};\n" ++
    "struct struct_" ++ name ++ " " ++ name ++ ";\n" ++
    "void init_" ++ name ++ "() {\n" ++
    (codeGen [x|x <- stmts, isAss x] name 0) ++
    "}\n"
    -- ++ (genTypDef [x | x <- stmts, isFunc x])

fancyGenStruct :: String -> FancyAST -> String
fancyGenStruct name stmts = "struct struct_" ++ name ++
    " {\n" ++ (genTypDef' [x|(env, x) <- stmts, isAss' (env, x)]) ++ "};\n" ++
    "struct struct_" ++ name ++ " " ++ name ++ ";\n" ++
    "void init_" ++ name ++ "() {\n" ++
    (fancyCodeGen Data.Map.empty [x|x <- stmts, isAss' x] name 1) ++
    "}\n"

forInitHelp :: VariablesMap -> FancyASTEntry -> String -> Int -> String
forInitHelp env (e, expr) klass depth = if (isAss' (e,expr)) then (genTypDefSingle env expr) else (fancyCodeGen Data.Map.empty [(e,expr)] klass 0)

--			ast    klassnamn    indent  code
codeGen :: [Expr] -> String -> Int -> String
codeGen [] _ _ = ""
codeGen ((Klass name stmts):ast) _ depth =
    (ind depth) ++ genStruct name stmts ++ (codeGen [x | x <- stmts, not (isAss x)] name depth) ++ "\n" ++ (codeGen ast "" depth)
codeGen ((BinaryOp name left right):ast) klass depth =
    (ind depth) ++ (codeGen [left] klass 0) ++ name ++ " " ++
    (codeGen [right] klass 0) ++ ";\n" ++
    (codeGen ast klass (depth))
codeGen ((Function t name params stmts):ast) klass depth =
    (ind depth) ++ (typestringToCtype t) ++ klass ++ "_" ++ name ++ "(" ++
    (join ", " [(typeToCtype typ) ++ name | (BinaryOp "=" (Var name) typ) <- params]) ++
    ") {\n" ++
    (genTypDef [x | x <- stmts, isAss x]) ++
    (codeGen stmts klass (depth+1)) ++ "\n}\n" ++
    (codeGen ast klass depth)
codeGen ((Var name):ast) klass depth =
    (ind depth) ++ klass ++ "." ++ name ++ " " ++ (codeGen ast klass depth)
codeGen ((Int value):ast) klass depth =
    (ind depth) ++ (show value) ++ (codeGen ast klass depth)
codeGen ((Float value):ast) klass depth =
    (ind depth) ++ (show value) ++ (codeGen ast klass depth)
codeGen ((String value):ast) klass depth =
    (ind depth) ++ (show value) ++ (codeGen ast klass depth)
codeGen ((If cond th el):ast) klass depth =
    (ind depth) ++ "if (" ++ (codeGen [cond] klass 0) ++ ") {\n" ++
    (genTypDef [x | x <- th, isAss x]) ++
    (join "\n" [codeGen [x] klass (depth+1) | x <- th]) ++ "\n" ++ (ind depth) ++
    "} else {\n" ++
    (genTypDef [x | x <- el, isAss x]) ++
    (join "\n" [codeGen [x] klass (depth+1)| x <- el]) ++
    (ind depth) ++ "}\n" ++
    (codeGen ast klass depth)
codeGen ((Return value):ast) klass depth =
    (ind depth) ++ "return " ++ (codeGen [value] klass 0) ++ ";\n" ++
    (codeGen ast klass depth)
codeGen ((Call cklass name params):ast) klass depth =
    (ind depth) ++ cklass ++ "_" ++ name ++ "(" ++
    (join ", " [codeGen [x] klass 0 | x <- params]) ++ ")" ++
    (if depth == 0 then "" else ";\n")  ++
    (codeGen ast klass depth)
codeGen (expr:ast) klass depth =
    (ind depth) ++ "other :: " ++ (show expr) ++ "\n" ++ (codeGen ast klass (depth+1))

fancyCodeGen :: FunctionsMap -> FancyAST -> String -> Int -> String
fancyCodeGen funcenv [] _ _ = ""
fancyCodeGen funcenv ((env, (KlassF name stmts)):ast) _ depth =
    (ind depth) ++ fancyGenStruct name stmts ++ (fancyCodeGen funcenv [x | x <- stmts, not (isAss' x)] name depth) ++ "\n" ++ (fancyCodeGen funcenv ast "" depth)
fancyCodeGen funcenv ((env, (BinaryOpF name left right)):ast) klass depth =
    (ind depth) ++ (fancyCodeGen funcenv [left] klass 0) ++ name ++ " " ++
    (fancyCodeGen funcenv [right] klass 0) ++ (if depth == 0 then "" else ";\n") ++
    (fancyCodeGen funcenv ast klass (depth))
fancyCodeGen funcenv ((env, (FunctionF t name params stmts)):ast) klass depth =
    (ind depth) ++ (typestringToCtype t) ++ klass ++ "_" ++ name ++ "(" ++
    (join ", " [(typeToCtype typ) ++ name | (BinaryOp "=" (Var name) typ) <- params]) ++
    ") {\n" ++
    (genTypDef' [x | (env, x) <- stmts, isAss' (env, x), not (inEnv env x)]) ++
    (fancyCodeGen funcenv stmts klass (depth+1)) ++ "\n}\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (VarF name)):ast) klass depth =
    (ind depth) ++ path ++ name ++ " " ++ (fancyCodeGen funcenv ast klass depth)
    where
        path = case (Data.Map.lookup (klass ++ "." ++ name) env) of
            Just x -> klass ++ "."
            Nothing -> ""
fancyCodeGen funcenv ((env, (IntF value)):ast) klass depth =
    (ind depth) ++ (show value) ++ (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (FloatF value)):ast) klass depth =
    (ind depth) ++ (show value) ++ (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (StringF value)):ast) klass depth =
    (ind depth) ++ (show value) ++ (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (IfF cond th el)):ast) klass depth =
    (ind depth) ++ "if (" ++ (codeGen [cond] klass 0) ++ ") {\n" ++
    (genTypDef' [x | (env, x) <- th, isAss' (env, x), not (inEnv env x)]) ++
    (join "\n" [fancyCodeGen funcenv [x] klass (depth+1) | x <- th]) ++ "\n" ++ (ind depth) ++
    "} else {\n" ++
    (genTypDef' [x | (env, x) <- el, isAss' (env, x), not (inEnv env x)]) ++
    (join "\n" [fancyCodeGen funcenv [x] klass (depth+1)| x <- el]) ++
    (ind depth) ++ "}\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env,(ForF init cond after fstmts)):ast) klass depth =
	(ind depth) ++ "for (" ++ (forInitHelp env init klass 0) ++ "; " ++
	(fancyCodeGen funcenv [cond] klass 0) ++ "; " ++
    (fancyCodeGen funcenv [after] klass 0) ++ ") {\n" ++
	(genTypDef' [x | (env, x) <- fstmts, isAss' (env, x), not (inEnv env x)]) ++
	(join "\n" [fancyCodeGen funcenv [x] klass (depth+1) | x <- fstmts]) ++ "\n" ++ (ind depth) ++
	(ind depth) ++ "}\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (ReturnF value)):ast) klass depth =
    (ind depth) ++ "return " ++ (codeGen [value] klass 0) ++ ";\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (CallF cklass name params)):ast) klass depth =
    (ind depth) ++ cklass ++ "_" ++ name ++ "(" ++
    (join ", " [fancyCodeGen funcenv [x] klass 0 | x <- params]) ++ ")" ++
    (if depth == 0 then "" else ";\n")  ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv (expr:ast) klass depth =
    (ind depth) ++ "other :: " ++ (show expr) ++ "\n" ++ (fancyCodeGen funcenv ast klass (depth+1))
