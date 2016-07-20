import Test.Hspec 
import Parser(compile, Expression(..), Op(..))
import Data.Maybe(fromJust)
import qualified Lexer as L
main :: IO ()
main = hspec $ do
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

  describe "syntax errors" $ do 
    it "resports errors" $ do
      errorsFor "(3 + )" `shouldBe`  "Unexpect token )"
