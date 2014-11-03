module PrettyPrinting where

import Syntax

join :: String -> [String] -> String
join _ [] = ""
join delim (x:[]) = x
join delim (x:xs) = x ++ delim ++ (join delim xs)

ind :: Int -> String
ind 0 = ""
ind x = if (x < 0) then "" else ((ind (x-1)) ++ "  ")

printClass :: String -> String
printClass "" = ""
printClass x = x ++ "§"


printAst :: [Expr] -> Int-> String
printAst [] _ = ""
printAst((Klass name expr):ast) depth =
    (ind depth) ++ "struktur " ++ name ++ "\n" ++
    (join "\n" ([printAst [i] (depth+1) | i <- expr])) ++
    "meep\n\n" ++
    (printAst ast depth)
printAst((Function t name params stmts):ast) depth =
    (ind depth) ++ t ++ " " ++ name ++ "(" ++ (printAst params 0) ++ ")\n" ++
    (join "\n" ([printAst [i] (depth+1) | i <- stmts])) ++ (ind depth) ++
    "klar\n\n" ++
    (printAst ast depth)
printAst((Var name):ast) depth =
    (ind depth) ++ name ++ " " ++
    (printAst ast depth)
printAst((BinaryOp name left right):ast) depth =
    (ind depth) ++ (printAst [left] 0) ++ " " ++ name ++ " " ++
    (printAst [right] 0) ++ " " ++
    (printAst ast depth)
printAst((Call klass name params):ast) depth =
    (ind depth) ++ (printClass klass) ++ name ++
    "(" ++ (printAst params 0) ++ ")" ++
    (printAst ast depth)
printAst((Float value):ast) depth =
    (ind depth) ++ (show value) ++
    (printAst ast depth)
printAst((Int value):ast) depth =
    (ind depth) ++ (show value) ++
    (printAst ast depth)
printAst((Async after before stmt):ast) depth =
    (ind depth) ++ "async efter " ++ (printAst [after] 0) ++ " före " ++
    (printAst [before] 0) ++ " " ++ (printAst [stmt] 0) ++
    (printAst ast depth)
printAst((If cond true false):ast) depth =
    (ind depth) ++ "om " ++ (printAst [cond] 0) ++ "\n" ++
    (printAst true (depth+1)) ++ "\n" ++ (ind depth) ++
    "annars\n" ++ (printAst false (depth+1)) ++ "\n" ++ (ind depth) ++
    "klar" ++
    (printAst ast depth)
printAst((For init cond after stmts):ast) depth =
    (ind depth) ++ "för " ++ (printAst [init] 0) ++ " " ++
    (printAst [cond] 0) ++ " " ++ (printAst [after] 0) ++ "\n" ++
    (join "\n" ([printAst [i] (depth+1) | i <- stmts])) ++ "\n" ++
    (ind depth) ++ "klar" ++
    (printAst ast depth)
printAst((String string):ast) depth =
    (ind depth) ++ "\"" ++ string ++ "\""
printAst((Void):ast) depth = ""
printAst((Return expr):ast) depth =
    (ind depth) ++ "återvänd " ++ printAst [expr] 0
printAst((Claim name stmts):ast) depth =
    (ind depth) ++ "begär " ++ name ++ "\n" ++
    (printAst stmts (depth+1)) ++ "\n" ++ (ind depth) ++ "klar" ++
    (printAst ast depth)
printAst((Include name _):ast) depth =
    "referera " ++ name ++ "\n\n" ++
    (printAst ast depth)
printAst((IncludeCore name stmts):ast) depth =
    "refereracore " ++ name ++ "\n" ++ (printAst stmts (depth+1)) ++ "meep\n\n" ++
    (printAst ast depth)
printAst (expr:ast) depth =
    (ind depth) ++ "other :: " ++ (show expr) ++
    (printAst ast depth)
