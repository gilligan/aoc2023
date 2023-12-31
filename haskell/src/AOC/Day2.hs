module AOC.Day2 where

import AOC.Util
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.List as L

data Stone = Blue | Red | Green
  deriving (Eq, Show, Ord)

type Set = [Draw]

type Game = [Set]

data Draw = Draw Integer Stone
  deriving (Eq, Show)

-- Parsing

parseStone :: Parser Stone
parseStone =
  symbol "red" $> Red
    <|> symbol "green" $> Green
    <|> symbol "blue" $> Blue

parseDraw :: Parser Draw
parseDraw = Draw <$> integer <*> parseStone

parseSet :: Parser Set
parseSet = sepBy1 parseDraw (symbol ",")

parseGame :: Parser Game
parseGame = string "Game " >> integer >> symbol ":" >> sepBy1 parseSet (symbol ";")

-- Part 1

isGamePossible :: Integer -> Integer -> Integer -> Game -> Bool
isGamePossible rMax gMax bMax = all (\set -> all (isDrawPossible set) set)
  where
    isDrawPossible :: Set -> Draw -> Bool
    isDrawPossible _ (Draw red Red)   = red <= rMax
    isDrawPossible _ (Draw green Green) = green <= gMax
    isDrawPossible _ (Draw blue Blue)   = blue <= bMax

part1 :: String -> IO (Maybe Int)
part1 filePath = p1 <$> readFile filePath

p1 :: String -> Maybe Int
p1 str = case traverse (parseString parseGame mempty) (lines str) of
  Success res -> Just $ sum $ (1+) <$> L.elemIndices True (isGamePossible 12 13 14 <$> res)
  Failure _ -> Nothing

-- Part 2

getPower :: Game -> Integer
getPower game =
  let (r, g, b) = foldr go (0, 0, 0) $ concat game
   in r * g * b
  where
    go (Draw x Red) (r, g, b) = (max r x, g, b)
    go (Draw x Green) (r, g, b) = (r, max g x, b)
    go (Draw x Blue) (r, g, b) = (r, g, max b x)

p2 :: String -> Maybe Integer
p2 str = case traverse (parseString parseGame mempty) (lines str) of
  Success res -> Just $ sum $ getPower <$> res
  Failure _ -> Nothing

part2 :: String -> IO (Maybe Integer)
part2 filePath = p2 <$> readFile filePath

