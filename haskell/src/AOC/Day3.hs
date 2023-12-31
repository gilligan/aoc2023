module AOC.Day3  where


-- blatantly copied from https://github.com/ephemient/aoc2023/blob/main/hs/src/Day3.hs
-- because I created a real mess and hated the puzzle and was too lazy to fix it

import Control.Arrow (second)
import Data.Char (isDigit, isSpace)
import qualified Data.Map as Map (elems, fromListWith)
import Data.Text (Text)
import qualified Data.Text as T (break, index, length, lines)
import qualified Data.Text.Read as T (decimal)
import qualified Data.Vector as V ((!), fromList, length)

parts :: Text -> [((Int, Int), Int)]
parts input =
  [ ((x, y'), number)
  | y <- [0..V.length lines - 1]
  , ((x0, x1), number) <- numbers 0 $ lines V.! y
  , y' <- [max 0 $ y - 1..min (V.length lines - 1) $ y + 1]
  , let line = lines V.! y'
  , x <- [max 0 $ x0 - 1..min (T.length line - 1) x1]
  , let c = T.index line x
  , not $ c == '.' || isSpace c || isDigit c
  ] where
    lines = V.fromList $ T.lines input
    numbers acc t
      | (leading, t') <- T.break isDigit t
      , Right (number, trailing) <- T.decimal t'
      = let acc' = acc + T.length t - T.length trailing
         in ((acc + T.length leading, acc'), number) : numbers acc' trailing
      | otherwise = []

part1 :: Text -> Int
part1 = sum . map snd . parts

part2 :: Text -> Int
part2 = sum . map product . filter ((== 2) . length) .
    Map.elems . Map.fromListWith (++) . map (second (:[])) . parts
