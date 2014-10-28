module TypeCheck where

import Syntax

import Data.Map
import Dictionary

typecheck :: [Expr] -> Data.Map.Map String String -> [String]
typecheck [] _ = []
typecheck ((Klass name expr):ast) env = typecheck (expr ++ ast) env
typecheck ((Function t name params stmts):ast) env = typecheck (params ++ stmts ++ ast) env -- checka även params mot typer, lägg in funktionstyp i env
typecheck ((Var name):ast) env = typecheck (ast) env
    where newenv = Data.Map.insert name "type" env
typecheck ((BinaryOp name left right):ast) env =
    (x):(typecheck (left:(right:ast)) newenv) -- lägg in i env vid =
    where
        (x, newenv) = case (name) of
            "=" -> case (left, right) of
                ((Var name),t) -> ("= OK " ++ name, Data.Map.insert name (typetostring t) env)
                otherwise -> ("= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ")", env)
            "+" -> case (left, right) of
                ((Int _),(Int _)) -> ("+ OK", env)
                ((Float _),(Float _)) -> ("+ OK", env)
                ((String _),(String _)) -> (" + OK", env)
                ((Var name),t) -> if ts==ts2 then ("+ OK", env) else ("+ ERROR: Unmatched types: (" ++ ts2 ++ ") (" ++ ts ++ ")", env)
                    where
                    ts = typetostring t
                    ts2 = case (Data.Map.lookup (name) env) of
                        Nothing -> "undeclared variable " ++ name
                        Just s -> s
                otherwise -> ("+ ERROR: Unmatched types: (" ++ (show left) ++ ") (" ++ (show right) ++ ")", env)
            otherwise -> ("Unhandled binaryop " ++ name, env)
typecheck ((Call name params):ast) env = typecheck (params ++ ast) env
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

typetostring :: Expr -> String
typetostring (String _) = "string"
typetostring (Int _) = "int"
typetostring (Float _) = "float"
typetostring (Void) = "void"
typetostring x = "unknown (" ++ (show x) ++ ")"
