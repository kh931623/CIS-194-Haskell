module WholemealSpec (spec) where

import Test.Hspec
import Wholemeal
  ( Tree (Leaf, Node),
    fun1,
    fun1',
    fun2,
    fun2',
    insert,
    map',
    xor,
  )

spec :: Spec
spec = do
  describe "fun1'" $ do
    it "should return the same result as fun1 when given [3, 4, 5, 6]" $ do
      fun1' [3, 4, 5, 6] `shouldBe` fun1 [3, 4, 5, 6]

  describe "fun2'" $ do
    it "should return the same result as fun2 when given 5" $ do
      fun2' 5 `shouldBe` fun2 5

  describe "xor" $ do
    it "should return True when given [False, True, False]" $ do
      xor [False, True, False] `shouldBe` True

    it "should return False when given [False, True, False, False, True]" $ do
      xor [False, True, False, False, True] `shouldBe` False

  describe "map'" $ do
    it "should return the same result as map when given (2*) [1, 2, 3, 4]" $ do
      map' (2 *) [1, 2, 3, 4] `shouldBe` map (2 *) [1, 2, 3, 4]

    it "should return [2, 3, 4, 5] when given (1+) [1, 2, 3, 4]" $ do
      map' (1 +) [1, 2, 3, 4] `shouldBe` [2, 3, 4, 5]
