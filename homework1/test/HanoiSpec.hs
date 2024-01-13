module HanoiSpec (spec) where

import Hanoi 
  (
    hanoi,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "hanoi function" $ do
    it "should return [('a','c'), ('a','b'), ('c','b')] when given 2 a b c" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
