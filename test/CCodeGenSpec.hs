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
