module UtilsSpec (spec) where

import Test.Hspec
import Utils
  ( takeEvery,
  )

spec :: Spec
spec = do
  describe "takeEvery" $ do
    it "should return [1, 2, 3, 4] when given 1 & [1, 2, 3, 4]" $ do
      takeEvery 1 [1, 2, 3, 4] `shouldBe` [1, 2, 3, 4]

    it "should return [2, 4] when given 2 & [1, 2, 3, 4]" $ do
      takeEvery 2 [1, 2, 3, 4] `shouldBe` [2, 4]

    it "should return [3] when given 3 & [1, 2, 3, 4]" $ do
      takeEvery 3 [1, 2, 3, 4] `shouldBe` [3]

    it "should return [] when given 5 & [1, 2, 3, 4]" $ do
      takeEvery 5 [1, 2, 3, 4] `shouldBe` []
