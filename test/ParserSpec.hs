module ParserSpec where
import Test.Hspec
import Types(Expression(..), Op(..))
import Parser(compile)
import Data.Maybe(fromJust)
import qualified Lexer as L

run :: Spec
run = do
  let generateAst = fromJust . fst . compile . L.process
  let errorsFor = snd . compile . L.process
  describe "terminals" $ do 
    it "identifies digits" $ do
      generateAst "3" `shouldBe` [Digit 3.0]

    it "identifies variables" $ do
      generateAst "hello" `shouldBe` [Var "hello"]

  describe "binary operators" $ do 
    it "generators a binary operator for addition" $ do
      generateAst "3 + 5" `shouldBe` [BinaryOp Add (Digit 3.0) (Digit 5.0)]

    it "generators a binary operator for less than" $ do
      generateAst "3 < 5" `shouldBe` [BinaryOp LessThan (Digit 3.0) (Digit 5.0)]

  describe "if then else" $
    it "supports conditional operations" $ do
      let result = If (BinaryOp (Add) (Digit 2.0) (Digit 3.0)) (Digit 1.0) (Digit 2.0)
      generateAst "if 2 + 3 then \n 1 \nelse\n 2" `shouldBe` [result]

  describe "functions" $ do
    it "supports functions with variables" $ do
      let result = Function "foo" ["a", "b"] (BinaryOp Add (Var "a") (Var "b"))
      generateAst "def foo(a, b)\n a + b" `shouldBe` [result]

  describe "for statement" $
    it "supports for statements" $ do
      let result = For "i" (Digit 0.0) (BinaryOp LessThan (Var "i") (Digit 3.0) ) (Digit 1.0) (Digit 5.0)
      generateAst "for i = 0, i < 3, 1 in\n 5" `shouldBe` [result]

  {- describe "syntax errors" $ do -}
    {- it "resports errors" $ do -}
      {- errorsFor "(3 + )" `shouldBe`  "Unexpect token )" -}
