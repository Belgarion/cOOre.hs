module TypeCheck where

import Syntax

import Data.Map
import Dictionary

type FunctionsMap = Data.Map.Map String String
type VariablesMap = Data.Map.Map String String

typecheck :: [Expr] -> FunctionsMap -> VariablesMap -> (FunctionsMap, [String])
typecheck [] funcenv _ = (funcenv, [])
typecheck ((Klass name expr):ast) funcenv env = typecheck (expr ++ ast) funcenv env
typecheck ((Function t name params stmts):ast) funcenv env = typecheck (params ++ stmts ++ ast) funcenv env -- checka även params mot typer, lägg in funktionstyp i env
typecheck ((Var name):ast) funcenv env = typecheck (ast) funcenv env
typecheck ((BinaryOp name left right):ast) funcenv env =
    (newfuncenv, (x):log) -- lägg in i env vid =
    where
        (newfuncenv, log) = (typecheck (left:(right:ast)) funcenv newenv)
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
typecheck ((Call _ name params):ast) funcenv env = typecheck (params ++ ast) funcenv env
typecheck ((Float value):ast) funcenv env = typecheck (ast) funcenv env
typecheck ((Int value):ast) funcenv env = typecheck (ast) funcenv env
typecheck ((Async after before stmt):ast) funcenv env = typecheck (stmt:ast) funcenv env
typecheck ((If cond true false):ast) funcenv env = typecheck (cond:(true ++ false ++ ast)) funcenv env
typecheck ((For init cond after stmts):ast) funcenv env = typecheck (init:(cond:(after:(stmts++ast)))) funcenv env
typecheck ((String string):ast) funcenv env = typecheck ast funcenv env
typecheck ((Void):ast) funcenv env = typecheck ast funcenv env
typecheck ((Return expr):ast) funcenv env = typecheck (expr:ast) funcenv env
typecheck ((Claim name stmts):ast) funcenv env = typecheck (stmts ++ ast) funcenv env
typecheck (expr:ast) funcenv env =
    (newfuncenv, ("Other " ++ (show expr)):log) -- lägg in i env vid =
    where
        (newfuncenv, log) = (typecheck ast newfuncenv env)

typetostring :: Expr -> Data.Map.Map String String -> String
typetostring (String _) _ = "string"
typetostring (Int _) _ = "int"
typetostring (Float _) _ = "float"
typetostring (Void) _ = "void"
typetostring (Var name) env = case (Data.Map.lookup (name) env) of
                        Nothing -> "undeclared variable " ++ name
                        Just s -> s
typetostring x _ = "unknown (" ++ (show x) ++ ")"
