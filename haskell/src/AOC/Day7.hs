{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module AOC.Day7 where

import AOC.Util
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Coerce (coerce)

sampleInput :: String
sampleInput =
  unlines
    [ "32T3K 765",
      "T55J5 684",
      "KK677 28",
      "KTJJT 220",
      "QQQJA 483"
    ]

-- different types of cards
data Card
  = Card2
  | Card3
  | Card4
  | Card5
  | Card6
  | Card7
  | Card8
  | Card9
  | CardT
  | CardJ
  | CardQ
  | CardK
  | CardA
  deriving (Eq, Enum, Bounded, Show)

instance Ord Card where
  compare c1 c2 = compare (toNum c1) (toNum c2)
    where
      toNum Card2 = 2
      toNum Card3 = 3
      toNum Card4 = 4
      toNum Card5 = 5
      toNum Card6 = 6
      toNum Card7 = 7
      toNum Card8 = 8
      toNum Card9 = 9
      toNum CardT = 10
      toNum CardJ = 11
      toNum CardQ = 12
      toNum CardK = 13
      toNum CardA = 14

newtype JkCard = JkCard { getCard :: Card }
  deriving (Show, Eq, Bounded)
  deriving Enum via Card

instance Ord JkCard where
  compare c1 c2 = compare (toNum $ getCard c1) (toNum $ getCard c2)
    where
      toNum CardJ = 1 
      toNum Card2 = 2
      toNum Card3 = 3
      toNum Card4 = 4
      toNum Card5 = 5
      toNum Card6 = 6
      toNum Card7 = 7
      toNum Card8 = 8
      toNum Card9 = 9
      toNum CardT = 10
      toNum CardQ = 12
      toNum CardK = 13
      toNum CardA = 14

data HType
  = High
  | OnePair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind
  deriving (Show, Eq, Ord, Enum, Bounded)

data Hand a = Hand a a a a a
  deriving (Eq)

newtype JkHand a = JkHand { getHand :: Hand a }
  deriving (Show, Eq)




instance (Show a) => Show (Hand a) where
  show (Hand c1 c2 c3 c4 c5) = "(" ++ show c1 ++ show c2 ++ show c3 ++ show c4 ++ show c5 ++ ")"

instance (Ord a) => Ord (Hand a) where
  compare h1@(Hand a1 a2 a3 a4 a5) h2@(Hand b1 b2 b3 b4 b5)
    | getHandType h1 == getHandType h2 = fromMaybe EQ (L.find (/= EQ) compared)
    | otherwise = compare (getHandType h1) (getHandType h2)
    where
      cards1 = [a1, a2, a3, a4, a5]
      cards2 = [b1, b2, b3, b4, b5]
      compared = zipWith compare cards1 cards2

instance (Ord a, a ~ JkCard) => Ord (JkHand a) where
  compare (JkHand h1@(Hand a1 a2 a3 a4 a5)) (JkHand h2@(Hand b1 b2 b3 b4 b5))
    | getHandTypeWithJoker (JkCard CardJ) h1 == getHandTypeWithJoker (JkCard CardJ) h2 = fromMaybe EQ (L.find (/= EQ) compared)
    | otherwise = compare (getHandTypeWithJoker (JkCard CardJ) h1) (getHandTypeWithJoker (JkCard CardJ) h2)
    where
      cards1 = [a1, a2, a3, a4, a5]
      cards2 = [b1, b2, b3, b4, b5]
      compared = zipWith compare cards1 cards2

aCard :: Parser Card
aCard =
  choice
    [ char '2' >> return Card2,
      char '3' >> return Card3,
      char '4' >> return Card4,
      char '5' >> return Card5,
      char '6' >> return Card6,
      char '7' >> return Card7,
      char '8' >> return Card8,
      char '9' >> return Card9,
      char 'A' >> return CardA,
      char 'K' >> return CardK,
      char 'Q' >> return CardQ,
      char 'J' >> return CardJ,
      char 'T' >> return CardT
    ]

aHand :: Parser (Hand Card)
aHand = Hand <$> aCard <*> aCard <*> aCard <*> aCard <*> aCard

handBid :: Parser (Hand Card, Integer)
handBid = (,) <$> aHand <*> (whiteSpace *> integer)

getHandTypeWithJoker :: (Ord a) => a -> Hand a -> HType
getHandTypeWithJoker joker h@(Hand c1 c2 c3 c4 c5)
  | handType == High && jokerCount == 1 = OnePair
  | handType == OnePair && jokerCount == 1 = ThreeKind
  | handType == OnePair && jokerCount == 2 = ThreeKind
  | handType == TwoPair && jokerCount == 1 = FullHouse
  | handType == TwoPair && jokerCount == 2 = FourKind
  | handType == ThreeKind && jokerCount == 1 = FourKind
  | handType == ThreeKind && jokerCount == 3 = FourKind
  | handType == FourKind && jokerCount == 1 = FiveKind
  | handType == FourKind && jokerCount == 4 = FiveKind
  | handType == FullHouse && jokerCount == 2 = FiveKind
  | handType == FullHouse && jokerCount == 3 = FiveKind
  | otherwise = handType
  where
    cards = L.sort [c1, c2, c3, c4, c5]
    handType = getHandType h
    jokerCount = L.length $ L.filter (== joker) cards

getHandType :: (Ord a) => Hand a -> HType
getHandType (Hand c1 c2 c3 c4 c5)
  | groups == [1, 1, 1, 1, 1] = High
  | groups == [1, 1, 1, 2] = OnePair
  | groups == [1, 2, 2] = TwoPair
  | groups == [1, 1, 3] = ThreeKind
  | groups == [2, 3] = FullHouse
  | groups == [1, 4] = FourKind
  | groups == [5] = FiveKind
  | otherwise = error "invalid combination"
  where
    groups = L.sort $ length <$> (L.group . L.sort $ cards)
    cards = L.sort [c1, c2, c3, c4, c5]

totalWinning :: [(Hand Card, Integer)] -> Integer
totalWinning input = L.sum $ L.zipWith getValue [1 ..] (L.sortOn fst input)
  where
    getValue pos (_, bid) = pos * bid

totalWinningJk :: [(JkHand JkCard, Integer)] -> Integer
totalWinningJk input = L.sum $ L.zipWith getValue [1 ..] (L.sortOn fst input)
  where
    getValue pos (_, bid) = pos * bid


p1 :: String -> Integer
p1 str = case traverse (parseString handBid mempty) (lines str) of
  Success input -> totalWinning input
  Failure err -> error $ show err

part1 :: IO Integer
part1 = do
  input <- readFile "./data/day7.txt"
  return $ p1 input

p2 :: String -> Integer
p2 str = case traverse (parseString handBid mempty) (lines str) of
  Success input -> totalWinningJk (coerce input :: [(JkHand JkCard, Integer)])
  Failure err -> error $ show err

part2 :: IO Integer
part2 = do
  input <- readFile "./data/day7.txt"
  return $ p2 input

