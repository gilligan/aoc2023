{-# LANGUAGE ImportQualifiedPost #-}
module AOC.Day8 where

import AOC.Util
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

-- AAA = (BBB, CCC)
sampleInput :: String
sampleInput =
  unlines
    [ "RL",
      "",
      "AAA = (BBB, CCC)",
      "BBB = (DDD, EEE)",
      "CCC = (ZZZ, GGG)",
      "DDD = (DDD, DDD)",
      "EEE = (EEE, EEE)",
      "GGG = (GGG, GGG)",
      "ZZZ = (ZZZ, ZZZ)"
    ]

sampleInput2 :: String
sampleInput2 =
  unlines
    [ "LLR",
      "",
      "AAA = (BBB, BBB)",
      "BBB = (AAA, ZZZ)",
      "ZZZ = (ZZZ, ZZZ)"
    ]

sampleInput3 :: String
sampleInput3 =
  unlines
    [ "LR",
      "",
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)"
    ]


-- data types

data Dir = R | L
  deriving (Eq, Show)

type Node = String

type Destination = String

type NodeMap = [(Node, (Node, Node))]

-- parsing

dirs :: Parser [Dir]
dirs =
  many
    ( choice
        [ L <$ char 'L',
          R <$ char 'R'
        ]
    )

aNode :: Parser String
aNode = count 3 (letter <|> digit)

connections :: Parser (String, String)
connections = between (symbol "(") (symbol ")") $ do
  nodeA <- aNode
  _ <- symbol ","
  nodeB <- aNode
  return (nodeA, nodeB)

nodeInput :: Parser (String, (String, String))
nodeInput = do
  node <- aNode
  _ <- whiteSpace
  _ <- symbol "="
  cs <- connections
  return (node, cs)

puzzleInput :: Parser ([Dir], [(String, (String, String))])
puzzleInput = do
  ds <- dirs <* whiteSpace
  nodes <- sepEndBy1 nodeInput whiteSpace
  return (ds, nodes)

-- puzzle logic

moveUntilAt :: [Dir] -> Node -> NodeMap -> Integer
moveUntilAt insts dest nodes = go "AAA" (head insts) 0
  where
    go :: Node -> Dir -> Integer -> Integer
    go curr move n =
      let (left, right) = fromMaybe (error "Could not find node!") (lookup curr nodes)
          count = n + 1
          nextNode = case move of
            L -> left
            R -> right
          instIndex = count `mod` toInteger (length insts)
          nextInst = insts !! fromInteger instIndex
       in if nextNode == dest then count else go nextNode nextInst count

getStartNodes :: NodeMap -> [Node]
getStartNodes nm = fst <$> filter (\(n, _) -> last n == 'A') nm


step :: Node -> Dir -> NodeMap -> Node
step node inst nodeMap =
  let (left, right) = fromMaybe (error "Could not find node!") (lookup node nodeMap)
   in case inst of
        L -> left
        R -> right

stepUntil :: [Dir] -> Node -> NodeMap -> Integer
stepUntil is startNode nodeMap = go startNode (head is) 0
  where
    go :: Node -> Dir -> Integer -> Integer
    go node move n
      | all (\x -> last x == 'Z') nexts = n + 1
      | otherwise =
        let count = n + 1
            instIndex = count `mod` toInteger (length is)
            nextInst = is !! fromInteger instIndex
            nextNode = step node move nodeMap
         in go nextNode nextInst count
      where
        nexts = [step node move nodeMap]

solvePart2 :: [Dir] -> NodeMap -> Integer
solvePart2 is nodeMap = foldl1 lcm [stepUntil is x nodeMap | x <- startNodes]
  where
    startNodes = getStartNodes nodeMap


part1 :: IO Integer
part1 = do
  inputData <- readFile "./data/day8.txt"
  return $ case parseString puzzleInput mempty inputData of
    Success (insts, nodeMap) -> moveUntilAt insts "ZZZ" nodeMap
    Failure err -> error $ show err

part2 :: IO Integer
part2 = do
  inputData <- readFile "./data/day8.txt"
  return $ case parseString puzzleInput mempty inputData of
    Success (insts, nodeMap) -> solvePart2 insts nodeMap
    Failure err -> error $ show err
