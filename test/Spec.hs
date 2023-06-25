import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Util

main :: IO ()
main = hspec $ do
  describe "Util.sumList" $ do
    it "return the sum of a list of integers inside a list" $ do
      sumList [1, 2, 3] `shouldBe` ([6] :: [Int])

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
