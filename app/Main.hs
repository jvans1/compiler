module Main where
import Lexer(process)
import System.Environment(getArgs)


main :: IO ()
main = do
  file:_ <- getArgs
  contents <- readFile file
  print $ process contents
