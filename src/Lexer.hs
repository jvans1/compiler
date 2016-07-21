module Lexer(process, Token(..)) where
import Control.Monad.Writer.Strict(Writer, runWriter, tell)
import Data.Maybe(fromJust)
import Data.List(takeWhile, dropWhile)
import Data.Char(isDigit, isLetter)

data Token = LParen
  | RParen
  | Add
  | Subtract
  | Multiply
  | Comma
  | Divide
  | Digit Double
  | Extern
  | Identifier String
  | If
  | Then
  | Else
  | For
  | In
  | Equal
  | LessThan
  | Def  deriving (Eq)


instance Show Token where
  show LParen         = "("
  show RParen         = ")"
  show Add            = "+"
  show If             = "if"
  show Then           = "then"
  show Else           = "else"
  show For            = "for"
  show In            = "in"
  show Equal          = "="
  show Subtract       = "-"
  show LessThan       = "<"
  show Multiply       = "*"
  show Comma          = "Comma"
  show Divide         = "/"
  show (Digit d)      = show d
  show Extern         = "extern"
  show (Identifier s) = s
  show Def            = "def"

process :: String -> [Token]
process input = snd . runWriter $ token input

token :: String -> Writer [Token] String
token input =
  case input of
    "" -> return []
    _  -> do
      let (mtok, restOfString) = parse input
      case mtok of
        Just tok -> tell [tok] >> token restOfString
        Nothing  -> token restOfString

parse :: String -> (Maybe Token, String)
parse [] = (Just LParen, "")
parse a@(x:xs) |
  isDigit x = (Just $ Digit (read $ takeWhile isDigit a), dropWhile isDigit a)
  | x  == '('   =  (Just LParen, xs)
  | x  == ')'   =  (Just RParen, xs)
  | x  == '+'   =  (Just Add, xs)
  | x  == '-'   =  (Just Subtract, xs)
  | x  == '='   =  (Just Equal, xs)
  | x  == '<'   =  (Just LessThan, xs)
  | x  == '*'   =  (Just Multiply, xs)
  | x  == ','   =  (Just Comma, xs)
  | x  == '/'   =  (Just Divide, xs)
  | x  == '\n'  =  (Nothing, xs)
  | x == '#'    =  (Nothing, dropWhile (/= '\n') xs) 
  | isLetter x  =  identifier a
  | otherwise   =  (Nothing, xs)


identifier :: String -> (Maybe Token, String)
identifier input = let str = takeWhile isLetter input
                       restOfString = dropWhile isLetter input in
                    case str of
                      "extern" -> (Just Extern, restOfString)
                      "def"    -> (Just Def, restOfString)
                      "if"     -> (Just If, restOfString)
                      "then"   -> (Just Then, restOfString)
                      "else"   -> (Just Else, restOfString)
                      "for"    -> (Just For, restOfString)
                      "in"    ->  (Just In, restOfString)
                      _        -> (Just (Identifier str), restOfString)
