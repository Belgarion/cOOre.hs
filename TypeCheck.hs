module TypeCheck where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace

--type FunctionsMap = Data.Map.Map String String
--type VariablesMap = Data.Map.Map String String

--type FancyAST = [(VariablesMap, FancyExpr)]

doTypecheck :: [Expr] -> (FancyAST, FunctionsMap, VariablesMap, [String])
doTypecheck ex = typecheck ex Data.Map.empty Data.Map.empty "" ""

typecheck :: [Expr] -> FunctionsMap -> VariablesMap -> String -> String -> (FancyAST, FunctionsMap, VariablesMap, [String])
typecheck [] funcenv env trace curclass = ([], funcenv, env, [])
typecheck ((Klass name expr):ast) funcenv env trace curclass =
    ((env, (KlassF name fast1)):fast2, nf2, env, nl1 ++ nl2)
    where
        (fast1, nf1, ne1, nl1) = typecheck (expr) funcenv env (newtrace) curclass
        (fast2, nf2, ne2, nl2) = typecheck (ast) funcenv env trace curclass
        newtrace = (trace ++ (if trace == "" then "" else "§") ++ name)
typecheck ((Function t name params stmts):ast) funcenv env trace curclass =
    ((env, (FunctionF t name params fast1)):fast2, env, env, nlparams ++ nl1 ++ nl2)
    where
        (fparams, nfparams, neparams, nlparams) = typecheck params newfuncenv env (newtrace) curclass
        (fast1, nf1, ne1, nl1) = typecheck stmts newfuncenv env (newtrace) curclass
        (fast2, nf2, ne2, nl2) = typecheck (ast) newfuncenv env trace curclass
        newtrace = (trace ++ (if trace == "" then "" else "§") ++ name)
        newfuncenv = Data.Map.insert name t funcenv
typecheck ((Var name):ast) funcenv env trace curclass =
    ((env, (VarF name)):fast, nf, ne, nl)
    where
      (fast, nf, ne, nl) = typecheck (ast) funcenv env trace curclass
typecheck ((BinaryOp name left right):ast) funcenv env trace curclass =
    ((env, (BinaryOpF name left right)):fast, newfuncenv2, env, if x == "" then (log ++ log2) else ((x):(log ++ log2)))
    where
        (fast, newfuncenv2, newenv3, log2) = (typecheck ast newfuncenv newenv2 trace curclass)
        (last_rast, newfuncenv, newenv2, log) = (typecheck (left:(right:[])) funcenv newenv trace curclass)
        (x, newenv) = case (name) of
            "=" -> case (left, right) of
                ((Var name),t) ->
                    case (Dictionary.lookupinsert name (typetostring t funcenv env) env) of
                        Just n -> ("", n)
                        Nothing -> if tl==tr then ("", env) else ("= ERROR Types not matching near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                            where
                                tl = typetostring (Var name) funcenv env
                                tr = typetostring t funcenv env
                otherwise -> ("= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ") near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
            "+" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "-" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("- ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "*" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("* ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            "/" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("/ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env
                    ts2 = typetostring t2 funcenv env
            otherwise -> ("Unhandled binaryop " ++ name, env)
typecheck ((Call klass name params):ast) funcenv env trace curclass =
    ((env, (CallF klass name params)):fast, funcenv, env, if error == "" then log else (error:log))
    where
        (fast, _, _, log) = typecheck (params ++ ast) funcenv env trace curclass
        error = case (Data.Map.lookup name funcenv) of
            Nothing -> "Undeclared function " ++ name
            Just s -> ""
typecheck ((Float value):ast) funcenv env trace curclass =
    typecheck (ast) funcenv env trace curclass
typecheck ((Int value):ast) funcenv env trace curclass =
    typecheck (ast) funcenv env trace curclass
typecheck ((Async after before stmt):ast) funcenv env trace curclass =
    typecheck (stmt:ast) funcenv env trace curclass
typecheck ((If cond true false):ast) funcenv env trace curclass =
    (((env, (IfF cond asttrue astfalse)):fast), funcenv, env, (nlcond ++ nltrue ++ nlfalse ++ nlast))
    where
        (astcond, nfcond, necond, nlcond) = typecheck [cond] funcenv env trace curclass
        (asttrue, nftrue, netrue, nltrue) = typecheck true funcenv env trace curclass
        (astfalse, nffalse, nefalse, nlfalse) = typecheck false funcenv env trace curclass
        (fast, nfast, neast, nlast) = typecheck ast funcenv env trace curclass
typecheck ((For init cond after stmts):ast) funcenv env trace curclass =
    typecheck (init:(cond:(after:(stmts++ast)))) funcenv env trace curclass
typecheck ((String string):ast) funcenv env trace curclass =
    typecheck ast funcenv env trace curclass
typecheck ((Void):ast) funcenv env trace curclass =
    typecheck ast funcenv env trace curclass
typecheck ((Return expr):ast) funcenv env trace curclass =
    ((env, (ReturnF expr)):fast, funcenv, env, nlexpr ++ nlast)
    where
        (exprast, nfexpr, neexpr, nlexpr) = typecheck [expr] funcenv env trace curclass
        (fast, nfast, neast, nlast) = typecheck ast funcenv env trace curclass
typecheck ((Claim name stmts):ast) funcenv env trace curclass =
    typecheck (stmts ++ ast) funcenv env trace curclass
typecheck ((Include filename stmts):ast) funcenv env trace curclass =
    typecheck (stmts ++ ast) funcenv env trace curclass
typecheck ((IncludeCore filename defs):ast) funcenv env trace curclass =
    typecheck (defs ++ ast) funcenv env trace curclass
typecheck (expr:ast) funcenv env trace curclass =
    (fast, newfuncenv, env, ("Other " ++ (show expr)):log)
    where
        (fast, newfuncenv, newenv, log) = (typecheck ast funcenv env trace curclass)

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
