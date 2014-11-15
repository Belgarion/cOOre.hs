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

import CodeGen

import Data.Map

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

processFile :: String -> IO ()
processFile fname = readFile fname >>= process


repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

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
            Right ex -> do
                putStrLn $ printAst ex 0
                let (fast, funcenv, varenv, log) = doTypecheck ex
                putStrLn $ join "\n" log
                let corecode = codeGen funcenv fast
                writeFile "out.core" corecode
                --putStrLn $ show fast
        return ()
