module FibonacciSpec (spec) where

import Fibonacci
  ( fib,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "fib" $ do
    it "should return 0 when given 0" $ do
      fib 0 `shouldBe` 0

    it "should return 1 when given 1" $ do
      fib 1 `shouldBe` 1

    it "should return 5 when given 5" $ do
      fib 5 `shouldBe` 5
