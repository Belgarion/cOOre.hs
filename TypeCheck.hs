module TypeCheck where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace

type FunctionsMap = Data.Map.Map String String
type VariablesMap = Data.Map.Map String String

doTypecheck :: [Expr] -> (FunctionsMap, VariablesMap, [String])
doTypecheck ex = typecheck ex Data.Map.empty Data.Map.empty "" ""

typecheck :: [Expr] -> FunctionsMap -> VariablesMap -> String -> String -> (FunctionsMap, VariablesMap, [String])
typecheck [] funcenv env trace curclass = (funcenv, env, [])
typecheck ((Klass name expr):ast) funcenv env trace curclass =
    typecheck (expr ++ ast) funcenv env (trace ++ (if trace == "" then "" else "§") ++ name) curclass
typecheck ((Function t name params stmts):ast) funcenv env  trace curclass=
    typecheck (params ++ stmts ++ ast) newfuncenv env trace curclass
    where
        newfuncenv = Data.Map.insert name t funcenv
    -- checka även params mot typer
typecheck ((Var name):ast) funcenv env trace curclass = typecheck (ast) funcenv env trace curclass
typecheck ((BinaryOp name left right):ast) funcenv env trace curclass =
    (newfuncenv2, env, (x):(log ++ log2))
    where
        (newfuncenv2, newenv3, log2) = (typecheck ast newfuncenv newenv2 trace curclass)
        (newfuncenv, newenv2, log) = (typecheck (left:(right:[])) funcenv newenv trace curclass)
        (x, newenv) = case (name) of
            "=" -> case (left, right) of
                ((Var name),t) -> ("= OK " ++ name, Data.Map.insert name (typetostring t funcenv env) env)
                otherwise -> ("= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ")", env)
            "+" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("+ OK", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "-" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("- OK", env) else ("- ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "*" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("* OK", env) else ("* ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "/" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("/ OK", env) else ("/ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            otherwise -> ("Unhandled binaryop " ++ name, env)
typecheck ((Call _ name params):ast) funcenv env trace curclass = typecheck (params ++ ast) funcenv env trace curclass
typecheck ((Float value):ast) funcenv env trace curclass = typecheck (ast) funcenv env trace curclass
typecheck ((Int value):ast) funcenv env trace curclass = typecheck (ast) funcenv env trace curclass
typecheck ((Async after before stmt):ast) funcenv env trace curclass = typecheck (stmt:ast) funcenv env trace curclass
typecheck ((If cond true false):ast) funcenv env trace curclass = typecheck (cond:(true ++ false ++ ast)) funcenv env trace curclass
typecheck ((For init cond after stmts):ast) funcenv env trace curclass = typecheck (init:(cond:(after:(stmts++ast)))) funcenv env trace curclass
typecheck ((String string):ast) funcenv env trace curclass = typecheck ast funcenv env trace curclass
typecheck ((Void):ast) funcenv env trace curclass = typecheck ast funcenv env trace curclass
typecheck ((Return expr):ast) funcenv env trace curclass = typecheck (expr:ast) funcenv env trace curclass
typecheck ((Claim name stmts):ast) funcenv env trace curclass = typecheck (stmts ++ ast) funcenv env trace curclass
typecheck ((Include filename stmts):ast) funcenv env trace curclass = typecheck (stmts ++ ast) funcenv env trace curclass
typecheck (expr:ast) funcenv env trace curclass =
    (newfuncenv, env, ("Other " ++ (show expr)):log)
    where
        (newfuncenv, newenv, log) = (typecheck ast newfuncenv env trace curclass)

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
    else ("Unmatched types : (" ++ (typetostring left funcenv env) ++ ") (" ++ (typetostring right funcenv env) ++ ")" ++ " near " ++ show(BinaryOp op left right))
typetostring (Call klass name params) funcenv env = -- if klass == "" inom samma klass
    case (Data.Map.lookup name funcenv) of
        Nothing -> "Undeclared function " ++ name
        Just s -> s
typetostring x _ _ = "unknown (" ++ (show x) ++ ")"
