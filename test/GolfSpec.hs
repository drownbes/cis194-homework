module GolfSpec where

import Golf

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "skips" $ do
    it "skips ABCD" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
    it "skips hello!" $ do
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    it "skips [1]" $ do
      skips [1] `shouldBe` [[1]]
    it "skips [True,False]" $ do
      skips [True, False] `shouldBe` [[True, False], [False]]
  describe "localMaxima" $ do
    it "localMaxima [2,9,5,6,1]" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
    it "localMaxima [2,3,4,1,5]" $ do
      localMaxima [2,3,4,1,5] `shouldBe` [4]
    it "localMaxima [1,2,3,4,5]" $ do
      localMaxima [1,2,3,4,5] `shouldBe` []
    it "localMaxima []" $ do
      localMaxima [] `shouldBe` []
    it "localMaxima [1,2]" $ do
      localMaxima [1,2] `shouldBe` []
