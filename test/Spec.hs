import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified ParserLib as P

successWithCharacter :: Char -> [Char] -> Bool
successWithCharacter x xs = (P.run (P.success x) xs) == (P.Success x xs) 

main :: IO ()
main = hspec $ do
  describe "success parser" $ do
    it "returns foo" $
      (P.run (P.success 'a') "abab") `shouldBe` (P.Success 'a' "abab") 
    it "should always return the passed in character" $ property successWithCharacter