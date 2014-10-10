module Types where

data COOREValue = Atom String
                | Bool Bool
                | Float Double
                | Number Integer
                | String String
                | Char Char
                | Func { params :: [String]
                        , vararg :: (Maybe String)
                        , body :: [COOREValue]
                        , closure :: Env
                        }
                | Class { environment :: Env
                        , funcs :: [Func]
                        }
instance Show COOREValue where show = showVal

data COOREError = NumArgs Integer [COOREValue]
                | ExpectCondClauses
                | ExpectCaseClauses
                | TypeMismatch String COOREValue
                | Parser ParseError
                | BadSpecialForm String COOREValue
                | NotFunction String String
                | UnboundVar String String
                | Default String
instance Show COOREError where show = showError

instance Error COOREError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either COOREError
type env = IORef [(String, IORef COOREValue)]
