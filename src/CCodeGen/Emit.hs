{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CCodeGen.Emit where
import Control.Monad.State
import Data.Map as Map
import Control.Applicative

import qualified Types as T

data Type = Double
data Function = Function Type Statement
data Statement = Addition Statement Statement | Digit Double

toC :: Function -> String
toC = error ""

data Code  = Code {
   functions :: Map.Map String Function
}
newtype CCode a = CCode { runCCode :: State Code a } deriving (Functor, Applicative, Monad, MonadState Code )

emit :: CCode () -> String
emit code = Map.foldrWithKey (\k v acc -> acc ++ "\n" ++ cFunction k v) "" $ f1 code

cFunction :: String -> Function -> String
cFunction name (Function ctype body) = (cType ctype) ++ " " ++ name ++ "()\n" ++ "{\n" ++ cBody body ++  "\n}\n"

cBody :: Statement -> String
cBody (Addition stm1 stm2) = " return " ++ (cBody stm1) ++ " + " ++ (cBody stm2)  ++ ";"
cBody (Digit d)            = (show d)

cType :: Type -> String
cType Double = "double"

f1 :: CCode () -> Map.Map String Function
f1 code = functions $ execState (runCCode code) (Code Map.empty)

codegen :: [T.Expression] -> CCode ()
codegen [] = return ()
codegen xs = mapM_ cgenTop xs

cgenTop :: T.Expression -> CCode ()
cgenTop (T.Function name args body) = do
  fns <- functions <$> get
  put $ Code $ Map.insert name (Function Double (cgen body)) fns
cgenTop x = error ("naked expression on top level, expected Function, got: " ++ show x)

cgen :: T.Expression -> Statement
cgen (T.BinaryOp T.Add expr1 expr2) = Addition (cgen expr1) (cgen expr2)
cgen (T.Digit d)                    = Digit d

