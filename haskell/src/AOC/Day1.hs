{-# LANGUAGE ImportQualifiedPost #-}

module AOC.Day1 (part1, p1, part2, p2, procLine, procLine', findOcurrences) where

import Data.Char qualified as C
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe qualified as M
import Data.Monoid

---- part 1

procLine :: String -> Integer
procLine str = read [go str, go $ reverse str]
  where
    go = M.fromJust . L.find C.isNumber

p1 :: String -> Integer
p1 str = getSum $ foldMap (Sum . procLine) $ lines str

part1 :: String -> IO Integer
part1 f = readFile f <&> p1

---- part 2

p2 :: String -> Integer
p2 str = getSum $ foldMap (Sum . procLine') $ lines str

part2 :: String -> IO Integer
part2 f = readFile f <&> p2

findOcurrences :: String -> String -> [(Int, String)]
findOcurrences subStr str = go str subStr 0 
  where
    go "" _ _ = []
    go s x n
      | take (length x) s == x = (n, x) : go (drop (length x) s) x (n + length x)
      | otherwise = go (tail s) x (n + 1)

procLine' :: String -> Integer
procLine' str = read [getNum $ head sorted, getNum $ last sorted]
  where
    getNum (_, "one") = '1'
    getNum (_, "two") = '2'
    getNum (_, "three") = '3'
    getNum (_, "four") = '4'
    getNum (_, "five") = '5'
    getNum (_, "six") = '6'
    getNum (_, "seven") = '7'
    getNum (_, "eight") = '8'
    getNum (_, "nine") = '9'
    getNum (_,x) = head x
    indices =
      concat $ sequence
        [ findOcurrences "one",
          findOcurrences "two",
          findOcurrences "three",
          findOcurrences "four",
          findOcurrences "five",
          findOcurrences "six",
          findOcurrences "seven",
          findOcurrences "eight",
          findOcurrences "nine",
          findOcurrences "1",
          findOcurrences "2",
          findOcurrences "3",
          findOcurrences "4",
          findOcurrences "5",
          findOcurrences "6",
          findOcurrences "7",
          findOcurrences "8",
          findOcurrences "9"
        ] str
    sorted = L.sortBy (\(a,_) (b,_) -> compare a b) indices
