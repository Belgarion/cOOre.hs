module TypeCheck where

import Syntax

type EnvType = (String, String)
type Env = [EnvType]

typecheck :: [Expr] -> Env -> [String]
typecheck [] _ = []
typecheck ((Klass name expr):ast) env = typecheck (expr ++ ast) env
typecheck ((Function t name params stmts):ast) env = typecheck (params ++ stmts ++ ast) env -- checka även params mot typer, lägg in funktionstyp i env
typecheck ((Var name):ast) env = typecheck (ast) env
typecheck ((BinaryOp name left right):ast) env =
    x:(typecheck (left:(right:ast)) env) -- lägg in i env vid =
    where
        x = case (name) of
            "=" -> case (left) of
                (Var _) -> "= OK"
                otherwise -> "= ERROR (" ++ (show left) ++ ") " ++ name ++ " ("++ (show right) ++ ")"
            otherwise -> name ++ " OK"
typecheck ((Call name params):ast) env = typecheck (params ++ ast) env
typecheck ((Float value):ast) env = typecheck (ast) env
typecheck ((Int value):ast) env = typecheck (ast) env
typecheck ((Async after before stmt):ast) env = typecheck (stmt:ast) env
typecheck ((If cond true false):ast) env = typecheck (cond:(true ++ false ++ ast)) env
typecheck ((For init cond after stmts):ast) env = typecheck (init:(cond:(after:(stmts++ast)))) env
typecheck ((String string):ast) env = typecheck ast env
typecheck ((Void):ast) env = typecheck ast env
typecheck ((Return expr):ast) env = typecheck (expr:ast) env
typecheck (expr:ast) env = ("Other " ++ (show expr)):(typecheck ast env)
