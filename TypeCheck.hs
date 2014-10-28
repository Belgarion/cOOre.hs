module TypeCheck where

import Syntax

import Data.Map
import Dictionary

typecheck :: [Expr] -> Data.Map.Map String String -> [String]
typecheck [] _ = []
typecheck ((Klass name expr):ast) env = typecheck (expr ++ ast) env
typecheck ((Function t name params stmts):ast) env = typecheck (params ++ stmts ++ ast) env -- checka även params mot typer, lägg in funktionstyp i env
typecheck ((Var name):ast) env = typecheck (ast) env
typecheck ((BinaryOp name left right):ast) env =
    (x):(typecheck (left:(right:ast)) newenv) -- lägg in i env vid =
    where
        (x, newenv) = case (name) of
            "=" -> case (left, right) of
                ((Var name),t) -> ("= OK " ++ name, Data.Map.insert name (typetostring t env) env)
                otherwise -> ("= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ")", env)
            "+" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("+ OK", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t env
                    ts2 = typetostring t2 env
            "-" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("+ OK", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t env
                    ts2 = typetostring t2 env
            "*" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("+ OK", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t env
                    ts2 = typetostring t2 env
            "/" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("+ OK", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t env
                    ts2 = typetostring t2 env
            otherwise -> ("Unhandled binaryop " ++ name, env)
typecheck ((Call _ name params):ast) env = typecheck (params ++ ast) env
typecheck ((Float value):ast) env = typecheck (ast) env
typecheck ((Int value):ast) env = typecheck (ast) env
typecheck ((Async after before stmt):ast) env = typecheck (stmt:ast) env
typecheck ((If cond true false):ast) env = typecheck (cond:(true ++ false ++ ast)) env
typecheck ((For init cond after stmts):ast) env = typecheck (init:(cond:(after:(stmts++ast)))) env
typecheck ((String string):ast) env = typecheck ast env
typecheck ((Void):ast) env = typecheck ast env
typecheck ((Return expr):ast) env = typecheck (expr:ast) env
typecheck ((Claim name stmts):ast) env = typecheck (stmts ++ ast) env
typecheck (expr:ast) env = ("Other " ++ (show expr)):(typecheck ast env)

typetostring :: Expr -> Data.Map.Map String String -> String
typetostring (String _) _ = "string"
typetostring (Int _) _ = "int"
typetostring (Float _) _ = "float"
typetostring (Void) _ = "void"
typetostring (Var name) env = case (Data.Map.lookup (name) env) of
                        Nothing -> "undeclared variable " ++ name
                        Just s -> s
typetostring x _ = "unknown (" ++ (show x) ++ ")"
