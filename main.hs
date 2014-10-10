
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "COORE>>> ") . evalAndPrint


main :: IO ()
main = do
   runRepl


