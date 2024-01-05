module CreditCardSpec (spec) where

import CreditCard (toDigitsRev)
import Test.Hspec

spec :: Spec
spec = do
  describe "toDigitsRev" $ do
    it "should return [] when given 0" $ do
      toDigitsRev 0 `shouldBe` []

    it "should return [4, 3, 2, 1] when given 1234" $ do
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

    it "should return [] when given negative numbers" $ do
      toDigitsRev (-17) `shouldBe` []
