module CCodeGenSpec where

import Test.Hspec
import Types(Expression(..), Op(..))
import Debug.Trace(trace)
import Parser(compile)
import Data.Maybe(fromJust)
import CCodeGen.Emit(codegen, emit)
import qualified Lexer as L

run :: Spec
run = do
  let generateAst = fromJust . fst . compile . L.process
  let errorsFor = snd . compile . L.process
  describe "generating c code" $ do
    it "generates c code for a function" $ do
      let ccode = "\ndouble foo()\n{\n return 3.0 + 5.0;\n}\n"
      let res = emit $ codegen [Function "foo" [] (BinaryOp Add (Digit 3.0) (Digit 5.0))]
      res `shouldBe` ccode

    it "passes arguments along to functions" $ do
      let ccode = "\ndouble foo(double a, double b)\n{\n return a + b;\n}\n"
      let res = emit $ codegen [Function "foo" ["a", "b"] (BinaryOp Add (Var "a") (Var "b"))]
      res `shouldBe` ccode

    it "recursively calls operations" $ do
      let ccode = "\ndouble foo()\n{\n return 1.0 + 2.0 + 3.0;\n}\n"
      let arg1 = (Digit 1.0)
      let arg2 = BinaryOp Add (Digit 2.0) (Digit 3.0)
      let res = emit $ codegen [Function "foo" [] (BinaryOp Add arg1 arg2)]
      res `shouldBe` ccode
