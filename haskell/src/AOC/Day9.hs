{-# LANGUAGE TypeApplications #-}
module AOC.Day9 where

readInput :: String -> [[Int]]
readInput = map (map (read @Int) . words) . lines

getNext :: [Int] -> Int
getNext xs
  | all (==0) xs = 0
  | otherwise = last xs + getNext diffs
  where
    diffs = uncurry (flip (-)) <$> zip xs (tail xs)

getNext' :: [Int] -> Int
getNext' xs
  | all (==0) xs = 0
  | otherwise = head xs - getNext' diffs
  where
    diffs = uncurry (flip (-)) <$> zip xs (tail xs)

part1 :: FilePath -> IO Int
part1 f = do
  input <- readInput <$> readFile f
  return $ sum $ getNext <$> input

part2 :: FilePath -> IO Int
part2 f = do
  input <- readInput <$> readFile f
  return $ sum $ getNext' <$> input
   
