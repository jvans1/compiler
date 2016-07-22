import Test.Hspec
import qualified ParserSpec
import qualified CCodeGenSpec
main :: IO ()
main = hspec $ do
  CCodeGenSpec.run
  ParserSpec.run
