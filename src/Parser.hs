module Parser(compile) where
import Control.Monad.Writer.Lazy(WriterT, runWriterT, tell, MonadWriter)
import Control.Monad.Trans(lift)
import Control.Applicative((<|>))
import Data.Maybe(Maybe, fromJust)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)
import Control.Monad.State.Lazy(evalState, State, get, put, MonadState)
import Lexer(Token)
import qualified Lexer as L
{-
  Grammar
  # Expr -> Term + Expr | Term - Expr | Keyword Ident FParamList Expr | Keyword Ident | Term
  # FParamList -> (FParamList, FParam) | FParam
  # FParam -> Ident | Ø
  # OptParams -> Params | Ø
  # Params -> Params, Param | Param
  # Param -> Expr
  # Term -> Factor * Term | Term / Expr | Factor
  # Factor -> Ident | Digit | (Expr) | Ident OptParams  
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

compile :: [Token] -> (Maybe [Expression], String)
compile tokens = evalState (runWriterT . runMaybeT $ many expression) tokens


expression :: Parser Expression
expression = addition <|> subtraction <|> functionDeclaration <|> extern <|> term

functionInvocation :: Parser Expression
functionInvocation = parseWithRollback $ do
  (Var fnName) <- identifier
  _      <- lparen
  exprs   <- many expression
  _      <- requireMatch rparen
  return (Call fnName exprs)


matchDef :: Parser ()
matchDef = do
  list <- get
  case list of
    (L.Def:xs) -> put xs
    _        -> fail ""

identifier :: Parser Expression
identifier = do
  list <- get
  case list of
    (L.Identifier i:xs) -> put xs >> return (Var i)
    _                   -> fail ""


functionDeclaration :: Parser Expression
functionDeclaration = parseWithRollback $ do
                        _ <- matchDef 
                        (Var fnName) <- requireMatch identifier
                        _ <- lparen
                        args <- many identifier
                        _ <- rparen
                        fnBody <- requireMatch expression
                        return (Function fnName args fnBody)
  


logError :: String -> Parser ()
logError err = tell (err ++ "\n\n")

parseWithRollback :: Parser Expression -> Parser Expression
parseWithRollback parser = do
            tokens <- get
            result <- lift $ runMaybeT parser
            case result of 
              Just a -> return a
              Nothing -> put tokens >> fail ""

matchAdd :: Parser ()
matchAdd = do
  list <- get
  case list of
    (L.Add:xs) -> put xs
    _        -> fail ""

requireMatch :: Parser a -> Parser a
requireMatch parser = do
  res <- lift $ runMaybeT parser
  case res of
    Just a  -> return a
    Nothing -> do 
      tokens <- get
      case tokens of
        []    -> tell "Syntax error, unexpected end of input\n" >> fail ""
        (x:_) -> tell ("Unexpected token: " ++ show x ++ "\n") >> fail ""
 

addition :: Parser Expression
addition = parseWithRollback $ do
              expr1 <- term
              _    <-  matchAdd
              expr2 <- requireMatch expression
              return (BinaryOp Add expr1 expr2)

multiplication :: Parser Expression
multiplication = parseWithRollback $ do
              expr1 <- factor
              _    <-  matchMultiply
              expr2 <- requireMatch term
              return (BinaryOp Multiply expr1 expr2)

division :: Parser Expression
division = parseWithRollback $ do
              expr1 <- factor
              _    <-  matchDivide
              expr2 <- requireMatch term
              return (BinaryOp Divide expr1 expr2)

factor :: Parser Expression
factor = functionInvocation <|> parenthesisExpr <|> identifier <|> digit

matchDivide :: Parser ()
matchDivide =  do
  list <- get
  case list of
    (L.Divide:xs) -> put xs
    _               -> fail ""

matchMultiply :: Parser ()
matchMultiply = do
  list <- get
  case list of
    (L.Multiply:xs) -> put xs
    _               -> fail ""


matchSubtract :: Parser ()
matchSubtract = do
  list <- get
  case list of
    (L.Subtract:xs) -> put xs
    _               -> fail ""

subtraction :: Parser Expression
subtraction = parseWithRollback $ do
              expr1 <- term
              _    <-  matchSubtract
              expr2 <- requireMatch expression
              return (BinaryOp Subtract expr1 expr2)

term :: Parser Expression
term  = multiplication <|> division <|> factor 

many :: Parser Expression -> Parser [Expression]
many parser = do
  res <- lift $ runMaybeT $ parseWithRollback parser
  case res of
    Just a -> many parser >>= return . (a:)
    Nothing -> return []

lparen :: Parser ()
lparen = do
  list <- get
  case list of
    (L.LParen:xs) -> put xs
    _               -> fail ""

rparen :: Parser ()
rparen = do
  list <- get
  case list of
    (L.RParen:xs) -> put xs
    _               -> fail ""

parenthesisExpr :: Parser Expression
parenthesisExpr = parseWithRollback $ do
                    _ <- lparen
                    expr <- expression
                    _ <- requireMatch rparen
                    return expr

matchExtern :: Parser ()
matchExtern = do
  list <- get
  case list of
    (L.Extern:xs) -> put xs
    _             -> fail ""

extern :: Parser Expression
extern = do
  _ <- matchExtern
  (Var name) <- identifier 
  exprs <- requireMatch $ many expression
  return (Extern name exprs)
  

digit :: Parser Expression
digit = do
  list <- get
  case list of
    (L.Digit f:xs) -> put xs >> return (Digit f)
    _              -> fail ""
