module CodeGen where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace
import System.IO.Unsafe

import PrettyPrinting (join, ind)

inEnv env (BinaryOpF "=" (envl, (VarF name)) (renv, right)) klass =
    case (Data.Map.lookup name env) of
        Just _ -> True
        Nothing -> case (Data.Map.lookup (klass ++ "." ++ name) env) of
            Just _ -> True
            Nothing -> False
inEnv env other klass = trace (show other) False

typeToCtype :: Expr -> String
typeToCtype (Int _) = "int "
typeToCtype (String _) = "char* "
typeToCtype (Float _) = "float "

typeToCtype' :: FancyExpr -> FunctionsMap -> String
typeToCtype' (IntF _) funcenv = "int "
typeToCtype' (StringF _) funcenv = "char* "
typeToCtype' (FloatF _) funcenv = "float "
typeToCtype' (CallF klass name _) funcenv =
    case (Data.Map.lookup (klass ++ "." ++ name) funcenv) of
        Nothing -> "trasigt"
        Just x -> typestringToCtype x
typeToCtype' x funcenv = "aoercuharocehuaroceuhaoeruch " ++ (show x) ++ "\n\n\n"

typestringToCtype :: Type -> String
typestringToCtype "hel" = "int "
typestringToCtype "sträng" = "char* "
typestringToCtype "flyt" = "float "
typestringToCtype "def" = "Task "
typestringToCtype x = "aeurchaoeurchaoeurch (" ++ x ++ ")\n\n\n"

printValue :: Expr -> String
printValue (Int value) = show value
printValue (Float value) = show value

-- generates typdefs for c structs and c function args
-- 			 ast	    defs
genTypDef' :: [FancyExpr] -> FunctionsMap -> String
genTypDef' [] funcenv = ""
genTypDef' ((BinaryOpF "=" (env, (VarF name)) (envr, typ)):vars) funcenv = (typeToCtype' typ funcenv) ++ name ++ ";\n" ++  (genTypDef' vars funcenv)
--genTypDef ((Function t name params stmts):x) = (typestringToCtype t) ++ name ++ "(" ++ (join ", " [(typeToCtype typ) ++ name | (BinaryOp "=" (Var name) typ) <- params]) ++ ");\n" ++ (genTypDef x)
genTypDef' (e) funcenv = "Other:: " ++ (show e)

genTypDefSingle :: FunctionsMap -> VariablesMap -> FancyExpr -> String -> String
genTypDefSingle funcenv env (BinaryOpF "=" (e,(VarF name)) (e2,typ)) curclass =
    typedef ++ name ++ " = " ++ (fancyCodeGen Data.Map.empty [(e2,typ)]  "" 0)
    where
        typedef = case (Data.Map.lookup name env) of
            Just x -> ""
            Nothing -> case (Data.Map.lookup (curclass ++ "." ++ name) env) of
                Just x -> ""
                Nothing -> (typeToCtype' typ funcenv)

-- checks if expr is an assignment
isAss' :: FancyASTEntry -> Bool
isAss' (_, (BinaryOpF "=" _ _)) = True
isAss' _ = False

-- generates klass-struct
--			klassname  ass 		struct
fancyGenStruct :: String -> FancyAST -> FunctionsMap -> String
fancyGenStruct name stmts funcenv = "struct struct_" ++ name ++
    " {\n" ++ (genTypDef' [x|(env, x) <- stmts, isAss' (env, x)] funcenv) ++ "};\n" ++
    "struct struct_" ++ name ++ " " ++ name ++ ";\n" ++
    "void init_" ++ name ++ "() {<#\n" ++
    (fancyCodeGen Data.Map.empty [x|x <- stmts, isAss' x] name (-1)) ++
    "#>}\n"

forInitHelp :: FunctionsMap -> VariablesMap -> FancyASTEntry -> String -> Int -> String
forInitHelp funcenv env (e, expr) klass depth = if (isAss' (e,expr)) then (genTypDefSingle funcenv env expr klass) else (fancyCodeGen Data.Map.empty [(e,expr)] klass 0)

--			ast    klassnamn    indent  code
fancyCodeGen :: FunctionsMap -> FancyAST -> String -> Int -> String
fancyCodeGen funcenv [] _ _ = ""
fancyCodeGen funcenv ((env, (KlassF name stmts)):ast) _ depth =
    (ind depth) ++ "#>" ++ (fancyGenStruct name stmts funcenv) ++ "<#" ++ (fancyCodeGen funcenv [x | x <- stmts, not (isAss' x)] name depth) ++ "\n" ++ (fancyCodeGen funcenv ast "" depth)
fancyCodeGen funcenv ((env, (BinaryOpF name left right)):ast) klass depth =
    (ind depth) ++ (if (depth == 0) then "" else "#>") ++ (fancyCodeGen funcenv [left] klass (if depth < 0 then depth else 0)) ++ name ++ " " ++
    (fancyCodeGen funcenv [right] klass 0) ++ (if depth == 0 then "" else ";<#\n") ++
    (fancyCodeGen funcenv ast klass (depth))
fancyCodeGen funcenv ((env, (FunctionF t name params stmts)):ast) klass depth =
    (ind depth) ++ (if t == "reset" then "" else (if (typestringToCtype t == "Task ") then "" else "Func ")
    ++ (typestringToCtype t) ++ klass ++ "_") ++
    name ++ (if t == "reset" then "" else ("(" ++
    (join ", " [(typeToCtype typ) ++ name | (BinaryOp "=" (Var name) typ) <- params]) ++
    ")")) ++ " {\n" ++
    "#>" ++ (genTypDef' [x | (env, x) <- stmts, isAss' (env, x), not (inEnv env x klass)] funcenv) ++ "<#" ++
    (fancyCodeGen funcenv stmts klass (depth+1)) ++ "\n}\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (VarF name)):ast) klass depth =
    (ind depth) ++ (if (depth == -1) then (klass ++ ".") else path) ++ name ++ " " ++ (fancyCodeGen funcenv ast klass depth)
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
    (ind depth) ++ "#>if (" ++ (fancyCodeGen funcenv [cond] klass 0) ++ ") {<#\n" ++
    "#>" ++ (genTypDef' [x | (env, x) <- th, isAss' (env, x), not (inEnv env x klass)] funcenv) ++ "<#" ++
    (join "\n" [fancyCodeGen funcenv [x] klass (depth+1) | x <- th]) ++ "\n" ++ (ind depth) ++
    "#>} else {<#\n" ++
    "#>" ++ (genTypDef' [x | (env, x) <- el, isAss' (env, x), not (inEnv env x klass)] funcenv) ++ "<#" ++
    (join "\n" [fancyCodeGen funcenv [x] klass (depth+1)| x <- el]) ++
    (ind depth) ++ "#>}<#\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env,(ForF init cond after fstmts)):ast) klass depth =
	(ind depth) ++ "#>for (" ++ (forInitHelp funcenv env init klass 0) ++ "; " ++
	(fancyCodeGen funcenv [cond] klass 0) ++ "; " ++
    (fancyCodeGen funcenv [after] klass 0) ++ ") {<#\n" ++
	(genTypDef' [x | (env, x) <- fstmts, isAss' (env, x), not (inEnv env x klass)] funcenv) ++
	(join "\n" [fancyCodeGen funcenv [x] klass (depth+1) | x <- fstmts]) ++ "\n" ++ (ind depth) ++
	(ind depth) ++ "#>}<#\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (ReturnF value)):ast) klass depth =
    (ind depth) ++ "#>return " ++ (fancyCodeGen funcenv [value] klass 0) ++ ";<#\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (CallF cklass name params)):ast) klass depth =
    (ind depth) ++ (isitC "#>") ++ maybeSync ++ (if cklass == "ext" then "" else (cklass ++ "_")) ++ name ++ "(" ++
    (join ", " [fancyCodeGen funcenv [x] klass 0 | x <- params]) ++ ")" ++
    (if depth == 0 then "" else ";" ++ (isitC "<#") ++ "\n")  ++
    (fancyCodeGen funcenv ast klass depth)
        where
            maybeSync = if (depth == 0) then "" else (if cklass == "ext" then "" else getSyncAsync)
            getSyncAsync = if getType == "def" then "async " else "sync "
            isitC str = if (depth == 0) then "" else (if cklass == "ext" then str else "")
            getType = case (Data.Map.lookup (if (cklass == "") then "" else (cklass ++ ".")  ++ name) funcenv) of
                Just x -> x
                Nothing -> ""
fancyCodeGen funcenv ((env, (IncludeCoreF name defs)):ast) klass depth =
    contents ++
    fancyCodeGen funcenv ast klass depth
    where
        contents = unsafePerformIO (readFile name)
fancyCodeGen funcenv ((env, (ClaimF name stmts)):ast) klass depth =
    (ind depth) ++ "claim " ++ name ++ " {\n" ++
	"#>" ++ (genTypDef' [x | (env, x) <- stmts, isAss' (env, x), not (inEnv env x klass)] funcenv) ++ "<#" ++
    (fancyCodeGen funcenv stmts klass depth) ++
    (ind depth) ++ "}\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (AsyncF after before stmt)):ast) klass depth =
    (ind depth) ++ "async " ++
    (if after == (Int 0) then "" else (" after " ++ (printValue after) ++ " ms ")) ++
    (if before == (Int 0) then "" else (" before " ++ (printValue before) ++ " ms ")) ++
    (fancyCodeGen funcenv [stmt] klass 0) ++
    (ind depth) ++ ";\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv ((env, (SyncF stmt)):ast) klass depth =
    (ind depth) ++ "sync " ++
    (fancyCodeGen funcenv [stmt] klass 0) ++
    (ind depth) ++ ";\n" ++
    (fancyCodeGen funcenv ast klass depth)
fancyCodeGen funcenv (expr:ast) klass depth =
    (ind depth) ++ "other :: " ++ (show expr) ++ "\n" ++ (fancyCodeGen funcenv ast klass (depth+1))
