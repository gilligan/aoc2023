{-# LANGUAGE DeriveFunctor #-}

module AOC.Day10 where

import AOC.Util
import Control.Applicative
import Data.Bifunctor
import Data.Maybe (fromJust)

-- data types
type TilePos = (Int, Int)

type TileMap = Grid Tile

data Tile
  = NS -- "|" is a vertical pipe connecting north and south.
  | EW -- "-" is a horizontal pipe connecting east and west.
  | NE -- "L" is a 90-degree bend connecting north and east.
  | NW -- "J" is a 90-degree bend connecting north and west.
  | SW -- "7" is a 90-degree bend connecting south and west.
  | SE -- "F" is a 90-degree bend connecting south and east.
  | Gnd -- .
  | S -- S
  deriving (Eq)

instance Show Tile where
  show NS = "|"
  show EW = "-"
  show NE = "L"
  show NW = "J"
  show SW = "7"
  show SE = "F"
  show Gnd = "."
  show S = "S"

data Connection a
  = OneWay a
  | TwoWay a a
  | None
  deriving (Eq, Show, Functor)

-- parser

aTile :: Parser Tile
aTile =
  choice
    [ NS <$ char '|',
      EW <$ char '-',
      NE <$ char 'L',
      NW <$ char 'J',
      SW <$ char '7',
      SE <$ char 'F',
      Gnd <$ char '.',
      S <$ char 'S'
    ]

-- utils

north :: (Int, Int)
north = (0, -1)

south :: (Int, Int)
south = (0, 1)

west :: (Int, Int)
west = (-1, 0)

east :: (Int, Int)
east = (1, 0)

tileToMoveOptions :: [(Tile, [(Int, Int)])]
tileToMoveOptions =
  [ (NS, [north, south]),
    (EW, [east, west]),
    (NE, [north, east]),
    (NW, [north, west]),
    (SW, [south, west]),
    (SE, [south, east]),
    (S, [north, east, south, west])
  ]

dirToTile :: [((Int, Int), [Tile])]
dirToTile =
  [ (west, [EW, SE, NE, S]), -- west
    (east, [NW, SW, EW, S]), -- east
    (north, [SW, NS, SE, S]), -- north
    (south, [NW, NS, NE, S]) -- south
  ]

getConnections :: TilePos -> TileMap -> Connection TilePos
getConnections pos@(x, y) tMap = case liftA2 zip possibleMoves validTiles of
  Nothing -> None
  Just movesAndTiles ->
    toConnection
      . fmap fst
      . filter (\x -> onMap x && isConnected x)
      . fmap (first toCoord)
      $ movesAndTiles
  where
    onMap (pos, _) = onGrid pos tMap
    toCoord = bimap (+x) (+y) 
    possibleMoves = lookup (tMap @ pos) tileToMoveOptions
    validTiles = possibleMoves >>= traverse (`lookup` dirToTile)
    isConnected (pos, ts) = (tMap @ pos) `elem` ts

findLoop :: TilePos -> TileMap -> [TilePos]
findLoop startPos tileMap = drop 2 $ reverse $ go startPos [startPos]
  where
    go pos path
      | tileMap @ pos == S && path /= [startPos] = path
      | otherwise = case getConnections pos tileMap of
          None -> []
          OneWay a -> go a (pos : path)
          TwoWay a b ->
            let prev = head path
                next = if a == prev then b else a
             in go next (pos : path)

toConnection :: (Show a) => [a] -> Connection a
toConnection [] = None
toConnection [x] = OneWay x
toConnection [l, r] = TwoWay l r
toConnection res = error $ "unexpected illegal connection" ++ show res

solvePart1 :: TileMap -> Int
solvePart1 map =
  let
    startPos = fromJust $ gridLocation S map
    path = findLoop startPos map
   in ((length path +1) `div` 2)

part1 :: IO Int
part1 = do
  input <- lines <$> readFile "./data/day10.txt"
  case traverse (parseString (many aTile) mempty) input of
    Success x -> return $ solvePart1 (Grid x)
    Failure err -> error $ show err
