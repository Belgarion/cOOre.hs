module TypeCheck where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace

type FunctionsMap = Data.Map.Map String String
type VariablesMap = Data.Map.Map String String


typecheck :: [Expr] -> FunctionsMap -> VariablesMap -> (FunctionsMap, VariablesMap, [String])
typecheck [] funcenv env = (funcenv, env, [])
typecheck ((Klass name expr):ast) funcenv env = typecheck (expr ++ ast) funcenv env
typecheck ((Function t name params stmts):ast) funcenv env =
    typecheck (params ++ stmts ++ ast) newfuncenv env
    where
        newfuncenv = Data.Map.insert name t funcenv
    -- checka även params mot typer
typecheck ((Var name):ast) funcenv env = typecheck (ast) funcenv env
typecheck ((BinaryOp name left right):ast) funcenv env =
    (newfuncenv2, env, (x):(log ++ log2)) -- lägg in i env vid =
    where
        (newfuncenv2, newenv3, log2) = (typecheck ast newfuncenv newenv2)
        (newfuncenv, newenv2, log) = (typecheck (left:(right:[])) funcenv newenv)
        (x, newenv) = case (name) of
            "=" -> case (left, right) of
                ((Var name),t) -> ("= OK " ++ name, Data.Map.insert name (typetostring t funcenv env) env)
                otherwise -> ("= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ")", env)
            "+" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("+ OK", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "-" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("- OK", env) else ("- ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "*" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("* OK", env) else ("* ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "/" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("/ OK", env) else ("/ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")", env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
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
    (newfuncenv, env, ("Other " ++ (show expr)):log) -- lägg in i env vid =
    where
        (newfuncenv, newenv, log) = (typecheck ast newfuncenv env)

typetostring :: Expr -> FunctionsMap -> VariablesMap-> String
typetostring (String _) _ _ = "sträng"
typetostring (Int _) _ _ = "hel"
typetostring (Float _) _ _ = "flyt"
typetostring (Void) _ _ = "def"
typetostring (Var name) funcenv env = case (Data.Map.lookup (name) env) of
                        Nothing -> "undeclared variable " ++ name
                        Just s -> s
typetostring (BinaryOp op left right) funcenv env =
    if (typetostring left funcenv env) == (typetostring right funcenv env)
    then (typetostring left funcenv env)
    else ("Unmatched types : (" ++ (typetostring left funcenv env) ++ ") (" ++ (typetostring right funcenv env) ++ ")")
typetostring (Call klass name params) funcenv env = -- if klass == "" inom samma klass
    case (Data.Map.lookup name funcenv) of
        Nothing -> "Undeclared function " ++ name
        Just s -> s
typetostring x _ _ = "unknown (" ++ (show x) ++ ")"
