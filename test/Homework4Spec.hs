module Homework4Spec where

import Homework4

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fun1" $ do
    it "fun1 equal fun1_new" $ do
      let x = [1,23,152,125,62]
      fun1 x == fun1_new x
  describe "fun2" $ do
    it "fun2 equal fun2_new" $ do
      let x = 10
      fun2 x == fun2_new x
  describe "foldTree" $ do
    it "should work" $ do
      foldTree [12,11,10,9,8,7,6,5,4,3,2,1] `shouldBe` (Node 4 (Node 6 (Node 2 (Node 0 Leaf 1 Leaf) 2 (Node 0 Leaf 3 Leaf)) 4 (Node 2 (Node 0 Leaf 5 Leaf) 6 (Node 0 Leaf 7 Leaf))) 8 (Node 2 (Node 0 Leaf 9 Leaf) 10 (Node 1 Leaf 11 (Node 0 Leaf 12 Leaf))))
  describe "xor" $ do
    it "should work" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

  describe "map'" $ do
    it "should work" $ do
      map (+1) [1,2,3] `shouldBe` map' (+1) [1,2,3]



