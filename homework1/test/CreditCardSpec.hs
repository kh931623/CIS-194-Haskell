module CreditCardSpec (spec) where

import CreditCard
  ( doubleEveryOther,
    sumDigits,
    testFunc,
    toDigits,
    toDigitsRev,
    validate,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "testFucn" $ do
    it "should return 'Hello' when given 1" $ do
      testFunc 1 `shouldBe` "Hello"

  describe "toDigitsRev" $ do
    it "should return [] when given 0" $ do
      toDigitsRev 0 `shouldBe` []

    it "should return [4, 3, 2, 1] when given 1234" $ do
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

    it "should return [] when given negative numbers" $ do
      toDigitsRev (-17) `shouldBe` []

  describe "toDigits" $ do
    it "should return [] when given 0" $ do
      toDigits 0 `shouldBe` []

  describe "doubleEveryOther" $ do
    it "should return [] when given []" $ do
      doubleEveryOther [] `shouldBe` []

    it "should return the input when given only one element" $ do
      doubleEveryOther [1] `shouldBe` [1]

    it "should return [2, 3] when given [1, 3]" $ do
      doubleEveryOther [1, 3] `shouldBe` [2, 3]

    it "should return [16, 7, 12, 5] when given [8, 7, 6, 5]" $ do
      doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]

    it "should return [1, 4, 3] when given [1, 2, 3]" $ do
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

  describe "sumDigits" $ do
    it "should return 22 when given [16, 7, 12, 5]" $ do
      sumDigits [16, 7, 12, 5] `shouldBe` 22

  describe "validate" $ do
    it "should return true when given 4012888888881881" $ do
      validate 4012888888881881 `shouldBe` True

    it "should return true when given 4012888888881882" $ do
      validate 4012888888881882 `shouldBe` False
