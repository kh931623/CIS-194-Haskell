module CalcSpec (spec) where

import Calc
  ( eval,
    evalStr,
  )
import ExprT
import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $ do
    it "should return 20 when given (Mul (Add (Lit 2) (Lit 3)) (Lit 4))" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $ do
    it "should return Maybe 20 when given (2+3) * 4" $ do
      evalStr "(2+3) * 4" `shouldBe` Just 20

    it "should return Nothing when given 2+3*" $ do
      evalStr "2+3*" `shouldBe` Nothing
