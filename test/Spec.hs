import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified ParserLib as P
import Token 

successWithCharacter :: Char -> [Char] -> Bool
successWithCharacter x xs = (P.run (P.success x) xs) == (P.Success x xs) 

successWithToken :: [Token] -> [Token] -> Bool 
successWithToken x xs = (P.run (P.success x) xs) == (P.Success x xs) 

main :: IO ()
main = hspec $ do
  describe "success parser" $ do
    it "should always return the passed in character" $ property successWithCharacter
    it "should always return the passed in token" $ property successWithToken