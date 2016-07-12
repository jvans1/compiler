module Parser(compile) where
import Control.Monad.Writer.Lazy(WriterT, runWriterT, tell, MonadWriter)
import Control.Monad.Base(liftBase)
import Control.Applicative((<|>))
import Data.Maybe(Maybe, fromJust)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)
import Control.Monad.State.Lazy(evalState, State, get, put, MonadState)
import Lexer(Token)
import qualified Lexer as L
{-
  Grammar
  # Expr -> Term + Expr | Term - Expr | Ident OptParams | Keyword Ident FParamList Expr | Term
  # FParamList -> (FParamList, FParam) | FParam
  # FParam -> Ident | Ø
  # OptParams -> Params | Ø
  # Params -> Params, Param | Param
  # Param -> Expr
  # Term -> Factor * Term | Term / Expr | Factor
  # Factor -> Ident | Digit | (Expr)
  # Keyword -> def | extern
-}
--    (3)

--          E
--        / | \
--       T  +  E
--     / |
--    F  *  T
--  /
--Ident
-- !Fail!
--          E
--        / | \
--       T  +  E
--     / |
--    F  *  T
--  /
--Digit
-- !Fail!
-- !Fail!
--              E
--            / | \
--           T  +  E
--         / |
--        F  *  T
--      / | \
--     (  E1  )
--     ^
--  !Success!
-- 
--  Move pointer to second lexeme, expand E1
--
--              E
--            / | \
--           T  +  E
--         / |
--        F  *  T
--      / | \
--     (  E  )
--      / | \
--    T   +  E
-- .... !Failure!
--
-- Roll pointer back to first lexeme
-- Repeat failures and rollbacks next 3
--
--              E
--            / | \
--           (  T   )
--           ^
--           |
--           matching terminal
--
-- Move pointer to next lexeme
--
--              E
--            / | \
--           (  T   )
--              |
--              F
--              |
--            Digit
--              ^
--              |
--              matching terminal
-- Move pointer to next lexeme
--
--              E
--            / | \
--           (  T   )
--              |   ^
--                  |
--                  matching terminal
--              F
--              |
--            Digit
-- Success!
-- No more non terminals for branch, all inputs matching
-- Move pointer to next lexeme
--
data Op = Add | Subtract | Divide | Multiply deriving Show
data Expression = BinaryOp Op Expression Expression
                  | Digit Float
                  | Var String
                  | Call String [Expression]
                  | Function String [Expression] Expression
                  | Extern String [Expression] deriving Show

type Parser = MaybeT (WriterT String (State [Token]))

compile :: [Token] -> (Maybe Expression, String)
compile tokens = evalState (runWriterT $ runMaybeT parse) tokens

parse :: Parser Expression
parse = addition <|> subtraction <|> term

logError :: String -> Parser ()
logError err = tell (err ++ "\n\n")

subtraction :: Parser Expression
subtraction = do
      list <- get
      case list of
        (L.LParen:xs) -> do
          put xs
          expr1 <- parse
          list <- get
          case list of
            (L.Subtract:xs2) -> do
              put xs2
              expr2 <- parse
              t2 <- get
              case t2 of
                (L.RParen:t3) -> do 
                  put t3
                  return (BinaryOp Subtract expr1 expr2)
                (x:xs)         -> do 
                  logError $ "Invalid token found: " ++ show x ++ ". Expected )"
                  recover xs
                  return (BinaryOp Subtract expr1 expr2)
            _ -> rollBack list 
        _ -> rollBack list

addition :: Parser Expression
addition = do
      list <- get
      case list of
        (L.LParen:xs) -> do
          put xs
          expr1 <- parse
          l1 <- get
          case l1 of
            (L.Add:xs2) -> do
              put xs2
              expr2 <- parse
              t2 <- get
              case t2 of
                (L.RParen:t3) -> do 
                  put t3
                  return (BinaryOp Add expr1 expr2)
                (x:xs)         -> do 
                  logError $ "Invalid token found: " ++ show x ++ ". Expected )"
                  recover xs
                  return (BinaryOp Add expr1 expr2)
            _ -> rollBack list 
        _ -> rollBack list


rollBack xs = put xs >> fail ""

recover :: [Token] -> Parser ()
recover = put . tail . dropWhile (not . isRParen)

isRParen :: L.Token -> Bool
isRParen L.RParen = True
isRParen _        = False

term ::  MaybeT (WriterT String (State [Token])) Expression
term  = do
  list <- get
  case list of
    (L.Digit f:xs) -> put xs >> return (Digit f)
    _              -> fail ""

factor :: [Token] -> Maybe Expression
factor xs = identifier xs <|> digit xs <|> parenthesisExpr xs

digit ::  [Token] -> Maybe Expression
digit = error "addition"

identifier ::  [Token] -> Maybe Expression
identifier = error "addition"

parenthesisExpr ::  [Token] -> Maybe Expression
parenthesisExpr = error "addition"

functionApplication ::  [Token] -> Maybe Expression
functionApplication = error "addition"

functionDefinition ::  [Token] -> Maybe Expression
functionDefinition = error "addition"

multiplication ::  [Token] -> Maybe Expression
multiplication = error "addition"

division ::  [Token] -> Maybe Expression
division = error "addition"

