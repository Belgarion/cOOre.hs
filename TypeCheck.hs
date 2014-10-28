module TypeCheck where

import Syntax

import Data.Map
import Dictionary

type EnvType = (String, String)
type Env = [EnvType]

typecheck :: [Expr] -> Data.Map.Map String String -> [String]
typecheck [] _ = []
typecheck ((Klass name expr):ast) env = typecheck (expr ++ ast) env
typecheck ((Function t name params stmts):ast) env = typecheck (params ++ stmts ++ ast) env -- checka även params mot typer, lägg in funktionstyp i env
typecheck ((Var name):ast) env = typecheck (ast) env
typecheck ((BinaryOp name left right):ast) env =
    (x++" "++y):(typecheck (left:(right:ast)) env) -- lägg in i env vid =
    where
        x = case (name) of
            "=" -> case (left) of
                (Var _) -> "= OK"
                otherwise -> "= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ")"
            otherwise -> name ++ " OK"
        (newmap, y) = case (name) of
            "=" -> case (right) of
                (Int _) -> case (Dictionary.lookupinsert (show left) "int" env) of
                    Nothing -> (env, "Already defined")
                    Just nmap -> (nmap, "")
                (Float _) -> case (Dictionary.lookupinsert (show left) "float" env) of
                    Nothing -> (env, "Already defined")
                    Just nmap -> (nmap, "")
                (String _) -> case (Dictionary.lookupinsert (show left) "string" env) of
                    Nothing -> (env, "Already defined")
                    Just nmap -> (nmap, "")
                otherwise -> (env, "Unsupported type: " ++ (show right))
            "+" -> case (left, right) of
                ((Int _),(Int _)) -> (env, "")
                ((Float _),(Float _)) -> (env, "")
                ((String _),(String _)) -> (env, "")
                ((Var _),t) -> if ts==ts2 then (env, "") else (env, "Unmatched types: " ++ ts2 ++ " " ++ ts)
                    where
                    ts = typetostring t
                    ts2 = case (Data.Map.lookup (show left) env) of
                        Nothing -> "undeclared variable"
                        Just s -> s
                otherwise -> (env, "Unmatched types: " ++ (show left) ++ " " ++ (show right))
            otherwise -> (env, "")
typecheck ((Call _ name params):ast) env = typecheck (params ++ ast) env
typecheck ((Float value):ast) env = typecheck (ast) env
typecheck ((Int value):ast) env = typecheck (ast) env
typecheck ((Async after before stmt):ast) env = typecheck (stmt:ast) env
typecheck ((If cond true false):ast) env = typecheck (cond:(true ++ false ++ ast)) env
typecheck ((For init cond after stmts):ast) env = typecheck (init:(cond:(after:(stmts++ast)))) env
typecheck ((String string):ast) env = typecheck ast env
typecheck ((Void):ast) env = typecheck ast env
typecheck ((Return expr):ast) env = typecheck (expr:ast) env
typecheck (expr:ast) env = ("Other " ++ (show expr)):(typecheck ast env)

typetostring :: Expr -> String
typetostring (String _) = "string"
typetostring (Int _) = "int"
typetostring (Float _) = "float"
typetostring (Void) = "void"
typetostring x = "unknown (" ++ (show x) ++ ")"
