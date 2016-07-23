module Parser(compile) where
import Control.Monad.Writer.Lazy(WriterT, runWriterT, tell, MonadWriter)
import Types
import Control.Monad(void)
import Control.Monad.Trans(lift)
import Control.Applicative((<|>))
import Data.Maybe(Maybe, fromJust)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)
import Control.Monad.State.Lazy(evalState, State, get, put, MonadState)
import Lexer(Token)
import qualified Lexer as L
{-
  Grammar
  # Expr -> Term + Expr | Term - Expr | If Expr Then Expr Else Expr | For String Expr Expr Expr Expr | Keyword Ident FParamList Expr | Keyword Ident | Term
  # FParamList -> (FParamList, FParam) | FParam
  # FParam -> Ident | Ø
  # OptParams -> Params | Ø
  # Params -> Params, Param | Param
  # Param -> Expr
  # Term -> Factor * Term | Term / Expr | Factor < Expr | Factor
  # Factor ->  Ident | Digit | (Expr) | Ident OptParams
  # Keyword -> def | extern | if | else | then
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
type Parser = MaybeT (WriterT String (State [Token]))

compile :: [Token] -> (Maybe [Expression], String)
compile tokens = evalState (runWriterT . runMaybeT $ many expression) tokens

expression :: Parser Expression
expression = addition <|> subtraction <|> functionDeclaration <|> ifthen <|> for <|> extern <|> term

matchFor :: Parser ()
matchFor = do
  list <- get
  case list of
    (L.For:xs) -> void (put xs)
    _        -> fail ""

matchEqual :: Parser ()
matchEqual = do
  list <- get
  case list of
    (L.Equal:xs) -> void (put xs)
    _        -> fail ""

matchComma :: Parser ()
matchComma = do
  list <- get
  case list of
    (L.Comma:xs) -> void (put xs)
    _        -> fail ""

matchIn :: Parser ()
matchIn = do
  list <- get
  case list of
    (L.In:xs) -> void (put xs)
    _        -> fail ""

for :: Parser Expression
for = do
  _ <- matchFor
  var <- requireMatch identifier
  _ <- requireMatch matchEqual
  start <- requireMatch expression
  _ <- requireMatch matchComma
  cond <- requireMatch expression
  _ <- requireMatch matchComma
  step <- requireMatch expression
  _ <- requireMatch matchIn
  body <- requireMatch expression
  return $ For var start cond step body
  
  


ifthen :: Parser Expression
ifthen = do
  _ <- matchIf
  conditional <- expression
  _ <- requireMatch matchThen
  expr1 <- requireMatch expression
  _ <- requireMatch matchElse
  expr2 <- requireMatch expression
  return (If conditional expr1 expr2)
  where
    matchIf :: Parser ()
    matchIf = do
      list <- get
      case list of
        (L.If:xs) -> void (put xs)
        _      -> fail ""

    matchThen :: Parser ()
    matchThen = do
      list <- get
      case list of
        (L.Then:xs) -> void (put xs)
        _        -> fail ""

    matchElse :: Parser ()
    matchElse = do
      list <- get
      case list of
        (L.Else:xs) -> void (put xs)
        _        -> fail ""



functionInvocation :: Parser Expression
functionInvocation = do
  expr <- parseWithRollback $ do
            fnName <- identifier
            _      <- lparen
            exprs <- many expression
            return (Call fnName exprs)
  _      <- requireMatch rparen
  return  expr


matchDef :: Parser ()
matchDef = do
  list <- get
  case list of
    (L.Def:xs) -> put xs
    _        -> fail ""

identifier :: Parser String
identifier = do
  list <- get
  case list of
    (L.Identifier i:xs) -> put xs >> return i
    _                   -> fail ""

functionVars :: Parser [String]
functionVars = do
  var <- identifier
  mcomma <- lift $ runMaybeT matchComma
  case mcomma of
    Just _ -> do
      rest <- functionVars
      return (var:rest)
    Nothing -> return [var]

functionDeclaration :: Parser Expression
functionDeclaration = parseWithRollback $ do
                        _ <- matchDef 
                        fnName <- requireMatch identifier
                        _ <- lparen
                        args <- functionVars
                        _ <- rparen
                        fnBody <- requireMatch expression
                        return (Function fnName args fnBody)

logError :: String -> Parser ()
logError err = tell (err ++ "\n\n")

parseWithRollback :: Parser a -> Parser a
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
        (x:xs) -> tell ("Unexpected token: " ++ show x ++ "\n") >> put xs >> fail ""

addition :: Parser Expression
addition = do
  expr <- parseWithRollback $ do
              expr1 <- term
              _    <-  matchAdd
              return expr1
  expr2 <- requireMatch expression
  return (BinaryOp Add expr expr2)

multiplication :: Parser Expression
multiplication = do
          expr <- parseWithRollback $ do
                    expr1 <- factor
                    _    <-  matchMultiply
                    return expr1
          expr2 <- requireMatch term
          return (BinaryOp Multiply expr expr2)

division :: Parser Expression
division = do
            expr <- parseWithRollback $ do
              expr1 <- factor
              _    <-  matchDivide
              return expr1
            expr2 <- requireMatch term
            return (BinaryOp Divide expr expr2)

factor :: Parser Expression
factor = functionInvocation <|> parenthesisExpr <|> digit <|> variable


lessThan :: Parser Expression
lessThan = do
  expr1 <- parseWithRollback $ do
    expr <- factor
    _ <- matchLT
    return expr
  expr2 <- requireMatch expression
  return $ BinaryOp LessThan expr1 expr2
  where
    matchLT :: Parser ()
    matchLT = do
      list <- get
      case list of
        (L.LessThan:xs)  -> void (put xs)
        _              -> fail ""

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
subtraction = do 
    expr <- parseWithRollback $ do
              expr1 <- term
              _    <-  matchSubtract
              return expr1
    expr2 <- requireMatch expression
    return (BinaryOp Subtract expr expr2)


variable :: Parser Expression
variable = parseWithRollback $ do
              ident <- identifier
              return (Var ident)

term :: Parser Expression
term  = multiplication <|> division <|> lessThan <|> factor  

many :: Parser a -> Parser [a]
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
parenthesisExpr = do
    expr <- parseWithRollback (lparen >> expression)
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
  name <- identifier
  _ <- lparen
  idents <- requireMatch $ many identifier
  _ <- requireMatch rparen
  return (Extern name idents)

digit :: Parser Expression
digit = do
  list <- get
  case list of
    (L.Digit f:xs) -> put xs >> return (Digit f)
    _              -> fail ""
