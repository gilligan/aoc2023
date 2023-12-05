module AOC.Day4  where

import AOC.Util
import qualified Data.List as L

data Card = Card
  { cId :: Integer,
    cWinners :: [Integer],
    cNumbers :: [Integer]
  }
  deriving (Eq, Ord, Show)

sample :: String
sample =
  unlines
    [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]

aCard :: Parser Card
aCard = do
  _ <- symbol "Card"
  cId <- integer <* symbol ":"
  winners <- nums
  _ <- symbol "|"
  Card cId winners <$> nums
  where
    nums = sepBy1 integer whiteSpace

scoreCard :: Card -> Int
scoreCard (Card _ wins nums) = case matches of
  0 -> 0
  1 -> 1
  n -> 2 ^ (n - 1)
  where
    matches = length $ L.intersect wins nums

part1 :: String -> Int
part1 str = case traverse (parseString aCard mempty) (lines str) of
  Success res -> sum $ scoreCard <$> res
  Failure _ -> error "oops"


processWith :: ((Integer, Integer) -> [Card]) -> [Card] -> [Card]
processWith _ [] = []
processWith get (c:cs) =  c : processWith get (wins ++ cs)
  where
    wins :: [Card]
    wins = get $ winRange c
    winRange :: Card -> (Integer, Integer)
    winRange (Card i w n) = (i, toInteger $ length $ L.intersect w n)

process :: [Card] -> [Card]
process deck = processWith getCards deck
  where
    getCards :: (Integer, Integer) -> [Card]
    getCards (index, count) = take (fromIntegral count) $ drop (fromIntegral index) deck

part2 :: String -> Int
part2 str = case traverse (parseString aCard mempty) (lines str) of
                 Success res -> length $ process res
                 Failure _ -> error "oops"
