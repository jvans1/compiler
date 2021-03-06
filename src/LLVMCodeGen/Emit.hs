module LLVMCodeGen.Emit where
import LLVMCodeGen.JIT
import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import LLVMCodeGen.CodeGeneration
import qualified Types as S

zero = cons $ C.Float (F.Double 0.0)

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

cgen :: S.Expression -> Codegen AST.Operand
cgen (S.Digit n) = return $ cons $ C.Float (F.Double n)
cgen (S.Var x) = getvar x >>= load
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs
cgen (S.BinaryOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error $ "no such operator" ++ (show op)
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  cond   <- cgen cond
  test   <- fcmp FP.ONE zero cond
  cbr test ifthen ifelse

  setBlock ifthen
  trval <- cgen tr
  br ifexit
  ifthen <- getBlock
  setBlock ifelse
  flval <- cgen fl
  br ifexit
  ifelse <- getBlock

  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]
cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  i <- alloca double
  istart <- cgen start
  stepval <- cgen step
  store i istart 
  assign ivar i
  br forloop

  setBlock forloop
  cgen body
  ival <- load i
  inext <- fadd ival stepval
  store i inext
  cond <- cgen cond
  test <- fcmp FP.ONE zero cond
  cbr test forloop forexit

  setBlock forexit
  return zero
  

binops = Map.fromList [(S.Add, fadd), (S.Subtract, fsub), (S.Divide, fdiv), (S.Multiply, fmul), (S.LessThan, lt)]

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

codegen :: AST.Module -> [S.Expression] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res of
    Right newast -> return newast
    Left err -> putStrLn err >> return oldast
  where
    modn = mapM codegenTop fns
    oldast = runLLVM mod modn

codegenTop :: S.Expression -> LLVM ()
codegenTop (S.Function name args body) =
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret


codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))
