module AOC.Day6 where

import AOC.Util
import Data.Functor ((<&>))

type RaceInfo = [(Integer, Integer)]

sampleInput :: String
sampleInput =
  unlines
    [ "Time:      7  15   30",
      "Distance:  9  40  200"
    ]

part2Input :: String
part2Input =
  unlines
    [ "Time:        53897698",
      "Distance:   313109012141201"
    ]

raceInput :: Parser RaceInfo
raceInput = zip <$> line <* whiteSpace <*> line
  where
    line = manyTill anyChar (char ':') *> whiteSpace *> integer `sepBy` spaces

waysToWin :: (Integer, Integer) -> Integer
waysToWin (time, distance) =
  toInteger . length $
    [ buttonPress * raceTime
      | buttonPress <- [1 .. (time-1)],
        raceTime <- [time - buttonPress],
        buttonPress * raceTime > distance
    ]

solveP1 :: String -> Integer
solveP1 str = case parseString raceInput mempty str of
  Success x -> product $ waysToWin <$> x
  Failure err -> error $ show err

part1 :: IO Integer
part1 = readFile "./data/day6.txt" <&> solveP1

part2 :: Integer
part2 = case parseString raceInput mempty part2Input of
  Success x -> product $ waysToWin <$> x
  Failure err -> error $ show err
