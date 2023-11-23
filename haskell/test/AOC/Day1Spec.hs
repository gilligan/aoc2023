module AOC.Day1Spec (spec) where

import AOC.Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    describe "exists" $ do
      it "should exist" $ do
        silly `shouldBe` "silly"
