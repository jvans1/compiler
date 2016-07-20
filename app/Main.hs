module Main where
import Lexer(process)
import Parser(compile)
import System.Environment(getArgs)
import CodeGeneration(emptyModule)
import Emit(codegen)

import qualified LLVM.General.AST as AST


initModule :: AST.Module
initModule = emptyModule "my cool jit"

main :: IO ()
main = do
  file:_ <- getArgs
  contents <- readFile file
  let lexingRes = (process contents)
  putStrLn "Lexing Results"
  putStrLn "=============="
  print lexingRes
  putStrLn "=============="
  let (expression, err) = compile lexingRes
  case expression of
    Just exs -> do
      case err of
        "" ->  do
          --No error successful parse
          putStrLn "Parsing Results"
          putStrLn "=============="
          print exs
          putStrLn "=============="
          codegen initModule exs
          return ()
        _  -> do
          --Errors that were recoverable, report to user
          putStrLn "Parse Failed: \n"
          putStrLn err
    Nothing -> do
     --Unrecoverable errors
      putStrLn "Parse Failed: \n"
      putStrLn err
