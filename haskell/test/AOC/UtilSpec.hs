module AOC.UtilSpec (spec) where

{- HLINT ignore "Use <$>" -}

import AOC.Util
import Test.Hspec

-- silly data structure for a string of the following format
-- DD.MM.YYYY
-- DD = day / MM = month / YYYY = year
data Datum = Datum
  { day :: Integer,
    month :: Integer,
    year :: Integer
  }
  deriving (Eq, Show)

parseDatum :: Parser Datum
parseDatum = do
  mm <- integer
  _ <- symbolic '.'
  dd <- integer
  _ <- symbolic '.'
  yy <- integer
  return $ Datum mm dd yy

spec :: Spec
spec = do
  describe "Util" $ do
    describe "parseDatum" $ do
      it "should parse 11.11.2021" $ do
        case parseString parseDatum mempty "11.11.2021" of
          Success d -> d `shouldBe` Datum 11 11 2021
          Failure _ -> expectationFailure "should have been a Success"
      it "should fail to parse xx.11.2021" $ do
        case parseString parseDatum mempty "xx.11.2021" of
          Success _ -> expectationFailure "should be a Failure"
          Failure _ -> pure ()
