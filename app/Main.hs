module Main where
import Lexer(process)
import Parser(compile)
import System.Environment(getArgs)


main :: IO ()
main = do
  file:_ <- getArgs
  contents <- readFile file
  let (expression, err) = compile (process contents) 
  case expression of
    Just ex -> do
      case err of
        "" ->  do
          --No error successful parse
          putStrLn "Succesful parse: \n"
          print expression
        _  -> do
          --Errors that were recoverable, report to user
          putStrLn "Parse Failed: \n"
          putStrLn err
    Nothing -> do
     --Unrecoverable errors
      putStrLn "Parse Failed: \n"
      putStrLn err
