--https://github.com/sdiehl/kaleidoscope/tree/master/src/chapter7
--http://www.stephendiehl.com/llvm/

module Main where

import Parser

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import TypeCheck
import PrettyPrinting

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop


processFile :: String -> IO ()
processFile fname = readFile fname >>= process

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> do
        file <- readFile fname
        process file
        let ast = parseToplevel file
        case ast of
            Left err -> print ""
            Right ex -> putStrLn (printAst ex 0)
        return ()

--import qualified LLVM.General.AST as AST

--initModule :: AST.Module
--initModule = emptyModule "my cool jit"

{-
process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop :: AST.Module -> InputT IO ()
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- lift $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod
-}
