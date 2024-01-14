module LogAnalysisSpec (spec) where

import Log
import LogAnalysis
  ( parseMessage,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should return LogMessage (Error 2) 562 'help help' when given 'E 2 562 help help'" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "should return LogMessage Info 29 'la la la' when given 'I 29 la la la'" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "should return a Unknown LogMessage when given wrong format input" $ do
      parseMessage "this format is wrong" `shouldBe` Unknown "this format is wrong"
