module TypeCheck where

import Syntax

import Data.Map
import Dictionary

import Debug.Trace

--type FunctionsMap = Data.Map.Map String String
--type VariablesMap = Data.Map.Map String String

--type FancyAST = [(VariablesMap, FancyExpr)]

firstFExpr (fast:_) = fast
firstFExpr [] = (Data.Map.empty, StringF "fail")

doTypecheck :: [Expr] -> (FancyAST, FunctionsMap, VariablesMap, [String])
doTypecheck ex = typecheck ex Data.Map.empty Data.Map.empty "" ""

typecheck :: [Expr] -> FunctionsMap -> VariablesMap -> String -> String -> (FancyAST, FunctionsMap, VariablesMap, [String])
typecheck [] funcenv env trace curclass = ([], funcenv, env, [])
typecheck ((Klass name expr):ast) funcenv env trace curclass =
    ((env, (KlassF name fast1)):fast2, nf2, env, nl1 ++ nl2)
    where
        (fast1, nf1, ne1, nl1) = typecheck (expr) funcenv env (newtrace) name
        (fast2, nf2, ne2, nl2) = typecheck (ast) nf1 env trace curclass
        newtrace = (trace ++ (if trace == "" then "" else "§") ++ name)
typecheck ((Function t name params stmts):ast) funcenv env trace curclass =
    ((env, (FunctionF t name params fast1)):fast2, nf2, env, nlparams ++ nl1 ++ nl2)
    where
        (fparams, nfparams, neparams, nlparams) = typecheck params newfuncenv env (newtrace) curclass
        (fast1, nf1, ne1, nl1) = typecheck stmts newfuncenv neparams (newtrace) curclass
        (fast2, nf2, ne2, nl2) = typecheck (ast) newfuncenv env trace curclass
        newtrace = (trace ++ (if trace == "" then "" else "§") ++ name)
        newfuncenv = Data.Map.insert (curclass ++ "." ++ name) t funcenv
typecheck ((Var name):ast) funcenv env trace curclass =
    ((env, (VarF name)):fast, nf, ne, nl)
    where
      (fast, nf, ne, nl) = typecheck (ast) funcenv env trace curclass
typecheck ((BinaryOp name left right):ast) funcenv env trace curclass =
    ((env, (BinaryOpF name (firstFExpr last) (firstFExpr rast) )):fast, newfuncenv2, newenv3, (if x == "" then [] else [x])++(logl ++ logr ++ log2))
    where
        (fast, newfuncenv2, newenv3, log2) = (typecheck ast newfuncenvv newenvv2 trace curclass)
        (rast, newfuncenvv, newenvv2, logr) = (typecheck ([right]) newfuncenv newenv2 trace curclass)
        (last, newfuncenv, newenv2, logl) = (typecheck ([left]) funcenv newenv trace curclass)
        (x, newenv) = case (name) of
            "=" -> case (left, right) of
                ((Var varname),t) ->
                    case (Dictionary.lookupinsert ((if trace == curclass then (curclass ++ ".") else "") ++varname) (typetostring t funcenv env curclass) env) of
                        Just n -> ("", n)
                        Nothing -> if tl==tr then ("", env) else ("= ERROR Types not matching near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                            where
                                tl = typetostring (Var varname) funcenv env curclass
                                tr = typetostring t funcenv env curclass
                otherwise -> ("= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ") near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
            "+" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("+ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env curclass
                    ts2 = typetostring t2 funcenv env curclass
            "-" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("- ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env curclass
                    ts2 = typetostring t2 funcenv env curclass
            "*" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("* ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env curclass
                    ts2 = typetostring t2 funcenv env curclass
            "/" -> case (left, right) of
                (t,t2) -> if ts==ts2 then ("", env) else ("/ ERROR: Unmatched types: (" ++ ts ++ ") (" ++ ts2 ++ ")" ++ " near " ++ trace ++ "§" ++ show(BinaryOp name left right), env)
                    where
                    ts = typetostring t funcenv env curclass
                    ts2 = typetostring t2 funcenv env curclass
            otherwise -> ("Unhandled binaryop " ++ name, env)
typecheck ((Call klass name params):ast) funcenv env trace curclass =
    ((env, (CallF cklass name past)):fast, funcenv, env, if error == "" then log else (error:log))
    where
        cklass = case (klass) of
            "ext" -> "ext"
            "" -> curclass
            otherwise -> case (Data.Map.lookup (klass ++ "." ++ name) funcenv) of
                Nothing -> ""
                Just s -> klass
        log = plog ++ flog
        (fast, _, _, flog) = typecheck (ast) funcenv env trace curclass
        (past, _, _, plog) = typecheck (params) funcenv env trace curclass
        error = case (Data.Map.lookup (cklass ++ "." ++ name) funcenv) of
            Nothing -> Debug.Trace.trace (show funcenv) $ "Undeclared function " ++ klass ++ "." ++ name
            Just s -> ""
typecheck ((Float value):ast) funcenv env trace curclass =
    ((env, (FloatF value)):fast, funcenv, env, log)
    where
        (fast, _, _, log) = typecheck (ast) funcenv env trace curclass
typecheck ((Int value):ast) funcenv env trace curclass =
    ((env, (IntF value)):fast, funcenv, env, log)
    where
        (fast, _, _, log) = typecheck (ast) funcenv env trace curclass
typecheck ((Async after before stmt):ast) funcenv env trace curclass =
    ((env, (AsyncF after before (firstFExpr iast))):fast, funcenv, env, nl1 ++ nl2)
    where
        (iast, _, _, nl1) = typecheck [stmt] funcenv env trace curclass
        (fast, _, _, nl2) = typecheck ast funcenv env trace curclass
typecheck ((Sync stmt):ast) funcenv env trace curclass =
    ((env, (SyncF (firstFExpr iast))):fast, funcenv, env, nl1 ++ nl2)
    where
        (iast, _, _, nl1) = typecheck [stmt] funcenv env trace curclass
        (fast, _, _, nl2) = typecheck ast funcenv env trace curclass

typecheck ((If cond true false):ast) funcenv env trace curclass =
    (((env, (IfF (firstFExpr astcond) asttrue astfalse)):fast), funcenv, env, (nlcond ++ nltrue ++ nlfalse ++ nlast))
    where
        (astcond, nfcond, necond, nlcond) = typecheck [cond] funcenv env trace curclass
        (asttrue, nftrue, netrue, nltrue) = typecheck true funcenv env trace curclass
        (astfalse, nffalse, nefalse, nlfalse) = typecheck false funcenv env trace curclass
        (fast, nfast, neast, nlast) = typecheck ast funcenv env trace curclass

typecheck ((For init cond after stmts):ast) funcenv env trace curclass =
    (((env,(ForF (firstFExpr iast) (firstFExpr cast) (firstFExpr aast) fstmts)):fast), funcenv, env, (inerr++coerr++aferr++sterr++nasterr))
    where
        (iast,_,ienv,inerr) = typecheck [init] funcenv env trace curclass
        (cast,_,_,coerr) = typecheck [cond] funcenv ienv trace curclass
        (aast,_,_,aferr) = typecheck [after] funcenv ienv trace curclass
        (fstmts, _, _, sterr) = typecheck stmts funcenv ienv trace curclass
        (fast, _, _, nasterr) = typecheck ast funcenv env trace curclass

typecheck ((String string):ast) funcenv env trace curclass =
    ((env, (StringF string)):fast, funcenv, env, log)
    where
        (fast, _, _, log) = typecheck (ast) funcenv env trace curclass
typecheck ((Void):ast) funcenv env trace curclass =
    ((env, (VoidF)):fast, funcenv, env, log)
    where
        (fast, _, _, log) = typecheck (ast) funcenv env trace curclass
typecheck ((Return expr):ast) funcenv env trace curclass =
    ((env, (ReturnF (firstFExpr exprast))):fast, funcenv, env, nlexpr ++ nlast)
    where
        (exprast, nfexpr, neexpr, nlexpr) = typecheck [expr] funcenv env trace curclass
        (fast, nfast, neast, nlast) = typecheck ast funcenv env trace curclass
typecheck ((Claim name stmts):ast) funcenv env trace curclass =
    ((env, (ClaimF name iast)):fast, newfuncenv, env, nl1 ++ nl2)
    where
        (fast, newfuncenv, _, nl1) = typecheck ast nf2 env trace curclass
        (iast, nf2, _, nl2) = typecheck stmts funcenv env trace curclass
typecheck ((Include filename stmts):ast) funcenv env trace curclass =
    typecheck (stmts ++ ast) funcenv env trace curclass -- TODO
typecheck ((IncludeCore filename defs):ast) funcenv env trace curclass =
    ((env, (IncludeCoreF filename iast)):fast, newfuncenv, env, nl1 ++ nl2)
    where
        (fast, newfuncenv, _, nl1) = (typecheck ast nf2 env trace curclass)
        (iast, nf2, _, nl2) = (typecheck defs funcenv env trace "ext")
typecheck (expr:ast) funcenv env trace curclass =
    (fast, newfuncenv, env, ("Other " ++ (show expr)):log)
    where
        (fast, newfuncenv, newenv, log) = (typecheck ast funcenv env trace curclass)

typetostring :: Expr -> FunctionsMap -> VariablesMap -> String -> String
typetostring (String _) _ _ _ = "sträng"
typetostring (Int _) _ _ _ = "hel"
typetostring (Float _) _ _ _ = "flyt"
typetostring (Void) _ _ _ = "def"
typetostring (Var name) funcenv env klass = case (Data.Map.lookup (name) env) of
                        Nothing -> case (Data.Map.lookup (klass ++ "." ++ name) env) of
                            Nothing ->"undeclared variable " ++ name
                            Just s -> s
                        Just s -> s
typetostring (BinaryOp op left right) funcenv env klass =
    if (typetostring left funcenv env klass) == (typetostring right funcenv env klass)
    then (typetostring left funcenv env klass)
    else ("Unmatched types : (" ++ (typetostring left funcenv env klass) ++ ") (" ++ (typetostring right funcenv env klass) ++ ")" ++ " near " ++ show(BinaryOp op left right))
typetostring (Call cklass name params) funcenv env klass = -- if klass == "" inom samma klass
    case (Data.Map.lookup (cklass ++ "." ++ name) funcenv) of
        Nothing -> "Undeclared function " ++ cklass ++ "." ++ name
        Just s -> s
typetostring x _ _ _ = "unknown (" ++ (show x) ++ ")"
