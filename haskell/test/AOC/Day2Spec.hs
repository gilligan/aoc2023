module AOC.Day2Spec (spec) where

import AOC.Day2
import Test.Hspec
import AOC.Util

samplePart1 :: String
samplePart1 = unlines [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]
 
spec :: Spec
spec = do
  describe "Day1" $ do
    describe "Parsing Business" $ do
      describe "parseStone" $ do
        it "parses all colors" $ do
          case traverse (parseString parseStone mempty) [ "red","green","blue"]  of
               Success x -> x `shouldBe` [Red, Green, Blue]
               Failure _ -> expectationFailure "failed to parse 'red'"
      describe "parseDraw" $ do
        it "parses combination of count and color" $ do
          case traverse (parseString parseDraw mempty) ["3 blue", "4 red", "2 green"] of
               Success x -> x `shouldBe` [Draw 3 Blue, Draw 4 Red, Draw 2 Green]
               Failure _ -> expectationFailure "failed to parse '3 blue'"
      describe "parseSet" $ do
        it "parses several draws" $ do
          case traverse (parseString parseSet mempty) ["3 blue, 4 red, 1 green"] of
               Success x -> x `shouldBe` [[Draw 3 Blue, Draw 4 Red, Draw 1 Green]]
               Failure _ -> expectationFailure "failed to parse Set"
      describe "parseGame" $ do
        it "parses several sets" $ do
          case traverse (parseString parseGame mempty) (lines samplePart1) of
               Success _ -> return ()
               Failure _ -> expectationFailure "failed to parse Game"

          case traverse (parseString parseGame mempty) ["Game 1: 3 blue, 4 red, 1 green; 3 blue, 4 red, 1 green"] of
               Success x -> x `shouldBe` [[[Draw 3 Blue, Draw 4 Red, Draw 1 Green], [Draw 3 Blue, Draw 4 Red, Draw 1 Green]]]
               Failure _ -> expectationFailure "failed to parse Game"

      describe "isDrawPossible" $ do
        it "checks if stones fall within limits" $ do
          isDrawPossible  10 10 10 (Draw 1 Red) `shouldBe` True
          isDrawPossible  10 10 10 (Draw 11 Red) `shouldBe` False
      describe "p1" $ do
        it "solves the sample input" $ do
          case p1 samplePart1 of
               Just res -> res `shouldBe` 8
               Nothing -> expectationFailure "p1 failed"
      describe "part1" $ do
        it "solves the puzzle input" $ do
          num <- part1 "./data/day2.txt"
          num `shouldBe` Just 2541
