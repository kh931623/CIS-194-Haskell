module LogAnalysisSpec (spec) where

import Log
import LogAnalysis
  ( insert,
    parseMessage,
  )
import Test.Hspec

logMsg1 :: LogMessage
logMsg1 = LogMessage Info 1 "hehe"

logMsg2 :: LogMessage
logMsg2 = LogMessage Info 2 "haha"

treeNode :: MessageTree
treeNode = Node Leaf logMsg2 Leaf

treeNode2 :: MessageTree
treeNode2 = Node Leaf logMsg1 Leaf

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should return LogMessage (Error 2) 562 'help help' when given 'E 2 562 help help'" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "should return LogMessage Info 29 'la la la' when given 'I 29 la la la'" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "should return a Unknown LogMessage when given wrong format input" $ do
      parseMessage "this format is wrong" `shouldBe` Unknown "this format is wrong"

  describe "insert" $ do
    it "should return original tree when given unknown log message" $ do
      insert (Unknown "hehe") Leaf `shouldBe` Leaf

    it "should return a new tree node when given a log message and a leaf node" $ do
      insert logMsg1 Leaf `shouldBe` Node Leaf logMsg1 Leaf

    it "should insert the log into the left tree when the timestamp of given log is less than the timestamp of given tree node" $ do
      insert logMsg1 treeNode `shouldBe` Node treeNode2 logMsg2 Leaf

    it "should insert the log into the right tree when the timestamp of given log is greater than the timestamp of given tree node" $ do
      insert logMsg2 treeNode2 `shouldBe` Node Leaf logMsg1 treeNode
