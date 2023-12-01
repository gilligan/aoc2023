module AOC.Day1Spec (spec) where

import AOC.Day1
import Test.Hspec

sampleInput :: String
sampleInput =
  unlines
    [ "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ]

samplePart2 :: String
samplePart2 =
  unlines
    [ "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    ]

spec :: Spec
spec = do
  describe "Day1" $ do
    describe "procLine" $ do
      it "processes a line correctly" $ do
        procLine (head $ lines sampleInput) `shouldBe` 12
        procLine (lines sampleInput !! 1) `shouldBe` 38
        procLine (lines sampleInput !! 2) `shouldBe` 15
        procLine (lines sampleInput !! 3) `shouldBe` 77
    describe "p1" $ do
      it "should yield right result for the sample data" $ do
        p1 sampleInput `shouldBe` 142
    describe "part1" $ do
      it "should solve the puzzle" $ do
        num <- part1 "./data/day1.txt"
        num `shouldBe` 54338
    describe "findOccurrences" $ do
      it "finds all indices" $ do
        findOcurrences "two" "two1nine" `shouldBe` [(0,"two")]
        findOcurrences "3" "three723thxk63" `shouldBe` [(7,"3"), (13,"3")]
    describe "procLine'" $ do
      it "extracts the right number" $ do
        procLine' "two1nine" `shouldBe` 29
        procLine' "eighttwothree" `shouldBe` 83
        procLine' "abcone2threexyz" `shouldBe` 13
        procLine' "xtwone3four" `shouldBe` 24
        procLine' "4nineeightseven2" `shouldBe` 42
        procLine' "zoneight234" `shouldBe` 14
        procLine' "7pqrstsixteen" `shouldBe` 76
    describe "p2" $ do
      it "yields correct value for sample input" $ do
        p2 samplePart2 `shouldBe` 281
    describe "part2" $ do
      it "should solve the puzzle" $ do
        num <- part2 "./data/day1.txt"
        num `shouldBe` 53389
