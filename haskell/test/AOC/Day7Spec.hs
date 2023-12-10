module AOC.Day7Spec (spec) where

import Test.Hspec
import AOC.Day7

spec :: Spec
spec = do
  describe "Day7" $ do
    describe "part1" $ do
      it "gives correct result" $ do
        res <- part1
        res `shouldBe` 246409899
    describe "part2" $ do
      it "gives correct result" $ do
        res <- part2
        res `shouldBe` 244848487

