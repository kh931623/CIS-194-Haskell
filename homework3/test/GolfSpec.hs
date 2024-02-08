module GolfSpec (spec) where

import Golf
  ( skips,
  )
import Test.Hspec

emptyList :: [Int]
emptyList = []

spec :: Spec
spec = do
  describe "skips" $ do
    it "should return [[1, 2, 3], [2], [3]] when given [1, 2, 3]" $ do
      skips [1, 2, 3] `shouldBe` [[1, 2, 3], [2], [3]]

    it "should return  [\"ABCD\", \"BD\", \"C\", \"D\"] when given \"ABCD\"" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]

    it "should return [] when given []" $ do
      skips emptyList `shouldBe` []
