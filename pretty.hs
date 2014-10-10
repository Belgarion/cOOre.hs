module PrettyPrinting where

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Char c) = ['\'', c, '\'']
showVal (PrimitiveFunc _) = "<primitive>"

showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
    "(lambda (" ++ unwords (map show args) ++
      (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispError -> String
showError ExpectCondClauses = "Expect at least 1 true cond clause"
showError ExpectCaseClauses = "Expect at least 1 true case clause"
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func

showError (NumArgs expected found) = "Expected " ++ show expected
                                   ++ " args; found values: " ++ unwordsList found

showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                   ++ ", found " ++ show found

showError (Parser parseErr) = "Parser error at " ++ show parseErr
