module LogAnalysisSpec where

import Log
import LogAnalysis
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "parse error message" $
      parseMessage "E 2 562 help help" `shouldBe`
      LogMessage (Error 2) 562 "help help"
    it "parse info message" $
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
    it "parse warning message" $
      parseMessage "W 562 help help" `shouldBe`
      LogMessage Warning 562 "help help"
    it "parse mark unknow messages" $
      parseMessage "This is not in the right format" `shouldBe`
      Unknown "This is not in the right format"
  describe "build" $ do
    it "binary tree from log messages list" $ do
      let logMsgs =
            [ LogMessage Info 1 "la la la"
            , LogMessage Info 0 "la la la"
            , LogMessage Info 3 "la la la"
            , LogMessage Info 5 "la la la"
            ]
      let result =
            Node
              (Node
                 (Node
                    Leaf
                    (LogMessage Info 0 "la la la")
                    (Node Leaf (LogMessage Info 1 "la la la") Leaf))
                 (LogMessage Info 3 "la la la")
                 Leaf)
              (LogMessage Info 5 "la la la")
              Leaf
      build logMsgs `shouldBe` result
  describe "inOrder" $ do
    it "return list of logs in order of timestamp" $ do
      let msgTree =
            Node
              (Node
                 (Node
                    Leaf
                    (LogMessage Info 0 "la la la")
                    (Node Leaf (LogMessage Info 1 "la la la") Leaf))
                 (LogMessage Info 3 "la la la")
                 Leaf)
              (LogMessage Info 5 "la la la")
              Leaf
      let result =
            [ LogMessage Info 0 "la la la"
            , LogMessage Info 1 "la la la"
            , LogMessage Info 3 "la la la"
            , LogMessage Info 5 "la la la"
            ]
      inOrder msgTree `shouldBe` result
