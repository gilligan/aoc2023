{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AOC.Day5 where

import AOC.Util
import qualified Data.List as L

data Range = Range { destStart :: Integer
                   , srcStart :: Integer
                   , length :: Integer
                   } deriving (Eq, Ord, Show)

type SeedMap = ((String, String), [Range])

sampleD5 :: String
sampleD5 =
  unlines
    [ "seeds: 79 14 55 13",
      "",
      "seed-to-soil map:",
      "50 98 2",
      "52 50 48",
      "",
      "soil-to-fertilizer map:",
      "0 15 37",
      "37 52 2",
      "39 0 15",
      "",
      "fertilizer-to-water map:",
      "49 53 8",
      "0 11 42",
      "42 0 7",
      "57 7 4",
      "",
      "water-to-light map:",
      "88 18 7",
      "18 25 70",
      "",
      "light-to-temperature map:",
      "45 77 23",
      "81 45 19",
      "68 64 13",
      "",
      "temperature-to-humidity map:",
      "0 69 1",
      "1 0 69",
      "",
      "humidity-to-location map:",
      "60 56 37",
      "56 93 4"
    ]

toPairs :: [a] -> [(a,a)]
toPairs [] = []
toPairs (a:b:rest) = (a,b) : toPairs rest


-- Parsing

initialSeeds :: Parser [Integer]
initialSeeds = string  "seeds: " *> sepBy1 integer whiteSpace

aSeedMap :: Parser SeedMap
aSeedMap = do
  fromto <- fromTo
  _ <- string " map:" <* newline
  ranges <- sepEndBy1 aRange whiteSpace
  return (fromto, ranges)
  where
    fromTo = do
      x <- some (noneOf "-")
      _ <- string "-to-"
      y <- some (noneOf " ")
      return (x,y)

aRange :: Parser Range
aRange = Range <$> integer
  <* whiteSpace
  <*> integer
  <* whiteSpace
  <*> integer

puzzleInput :: Parser ([Integer], [SeedMap])
puzzleInput = do
  inits <- initialSeeds
  maps <- sepEndBy1 aSeedMap whiteSpace
  return (inits, maps)

-- Utils

part1 :: String -> IO Integer
part1 str = do
  contents <- readFile str
  return $ case parseString puzzleInput mempty contents of
       Success (seeds, maps) -> findClosest seeds maps
       Failure err -> error $ show err

part2 :: String -> IO Integer
part2 str = do
  contents <- readFile str
  return $ case parseString puzzleInput mempty contents of
       Success (seeds, maps) -> minimum $ closestFromRange seeds maps
       Failure err -> error $ show err

closestFromRange :: [Integer] -> [SeedMap] -> [Integer]
closestFromRange seeds maps = lookup maps <$> (findClosestInRange maps <$> ranges)
  where
    ranges = toPairs seeds
    lookup :: [SeedMap] -> Integer -> Integer
    lookup ms n = foldl mapNumber n ms

findClosestInRange :: [SeedMap] -> (Integer, Integer) -> Integer
findClosestInRange maps (start, len) = L.minimumBy cmp [start..start+len]
  where
    cmp a b = compare (lookup a maps) (lookup b maps)
    lookup :: Integer -> [SeedMap] -> Integer
    lookup = foldl mapNumber

buildRanges :: [Integer] -> [Integer]
buildRanges seeds = concatMap (\(start,len) -> [start..(start+len)]) $ toPairs seeds


findClosest :: [Integer] -> [SeedMap] -> Integer
findClosest nums maps = minimum $ lookupNum maps <$> nums
  where
    lookupNum maps n = foldl mapNumber n maps


mapNumber :: Integer -> SeedMap -> Integer
mapNumber n (_, rs) = case L.find (inRange n) rs of
                           Nothing -> n
                           Just (Range dst src _) -> dst + (n - src)
  where
    inRange :: Integer -> Range -> Bool
    inRange num (Range _ src len)
      | num < src || num > (src + len-1) = False
      | otherwise = True

day :: Int
day = 5
