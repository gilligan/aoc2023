module AOC.Day8Spec (spec, shouldParseTo) where

import AOC.Day8
import AOC.Util
import Test.Hspec

parsedSampleInput :: ([Dir], [(String, (String, String))])
parsedSampleInput =
  ( [R, L],
    [ ("AAA", ("BBB", "CCC")),
      ("BBB", ("DDD", "EEE")),
      ("CCC", ("ZZZ", "GGG")),
      ("DDD", ("DDD", "DDD")),
      ("EEE", ("EEE", "EEE")),
      ("GGG", ("GGG", "GGG")),
      ("ZZZ", ("ZZZ", "ZZZ"))
    ]
  )

shouldParseTo :: (Show a, Eq a) => Result a -> a -> Expectation
shouldParseTo actual expected = case actual of
  Success res -> res `shouldBe` expected
  Failure err -> expectationFailure $ show err

spec :: Spec
spec = do
  describe "Day8" $ do
    describe "parsing" $ do
      it "parses a node line" $ do
        parseString nodeInput mempty "AAA = (BBB, CCC)" `shouldParseTo` ("AAA", ("BBB", "CCC"))
      it "parses turns" $ do
        parseString dirs mempty "LRLR" `shouldParseTo` [L, R, L, R]
      it "parses puzzle input" $ do
        parseString puzzleInput mempty sampleInput `shouldParseTo` parsedSampleInput
    describe "moveUntilAt" $ do
      it "should solve for part1 sampleInput" $ do
        moveUntilAt (fst parsedSampleInput) "ZZZ" (snd parsedSampleInput) `shouldBe` 2
