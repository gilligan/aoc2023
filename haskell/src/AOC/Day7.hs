{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module AOC.Day7 where

import AOC.Util
import Data.Coerce (coerce)
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe)

sampleInput :: String
sampleInput =
  unlines
    [ "32T3K 765",
      "T55J5 684",
      "KK677 28",
      "KTJJT 220",
      "QQQJA 483"
    ]

-- Data Types for cards and a hand of cards
 
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

data Hand a = Hand a a a a a
  deriving (Show, Eq)

newtype JkHand a = JkHand {getHand :: Hand a}
  deriving (Show, Eq)

newtype JkCard = JkCard {getCard :: Card}
  deriving (Show, Eq, Bounded)
  deriving (Enum) via Card

data HType
  = High
  | OnePair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind
  deriving (Show, Eq, Ord, Enum, Bounded)

-- instances

cardToNum :: Card -> Int
cardToNum c = (2+) $ fromJust $ L.elemIndex c [minBound .. maxBound]

instance Ord Card where
  compare c1 c2 = compare (cardToNum c1) (cardToNum c2)

instance Ord JkCard where
  compare c1 c2 = compare (toNum $ getCard c1) (toNum $ getCard c2)
    where
      toNum CardJ = 1
      toNum x = cardToNum x

instance (Ord a) => Ord (Hand a) where
  compare h1@(Hand a1 a2 a3 a4 a5) h2@(Hand b1 b2 b3 b4 b5)
    | sameHand = fromMaybe EQ (L.find (/= EQ) compared)
    | otherwise = compare (getHandType h1) (getHandType h2)
    where
      sameHand = getHandType h1 == getHandType h2
      compared = zipWith compare [a1, a2, a3, a4, a5] [b1, b2, b3, b4, b5]

instance Ord (JkHand JkCard) where
  compare (JkHand h1@(Hand a1 a2 a3 a4 a5)) (JkHand h2@(Hand b1 b2 b3 b4 b5))
    | sameHand = fromMaybe EQ (L.find (/= EQ) compared)
    | otherwise = compare (getHandTypeWithJoker joker h1) (getHandTypeWithJoker joker h2)
    where
      joker = JkCard CardJ
      sameHand = getHandTypeWithJoker joker h1 == getHandTypeWithJoker joker h2
      compared = zipWith compare [a1, a2, a3, a4, a5] [b1, b2, b3, b4, b5]

handWithBid :: Parser (Hand Card, Integer)
handWithBid = (,) <$> aHand <*> (whiteSpace *> integer)
  where
    aHand :: Parser (Hand Card)
    aHand = Hand <$> aCard <*> aCard <*> aCard <*> aCard <*> aCard
    aCard :: Parser Card
    aCard = choice
      [ Card2 <$ char '2',
        Card3 <$ char '3',
        Card4 <$ char '4',
        Card5 <$ char '5',
        Card6 <$ char '6',
        Card7 <$ char '7',
        Card8 <$ char '8',
        Card9 <$ char '9',
        CardA <$ char 'A',
        CardK <$ char 'K',
        CardQ <$ char 'Q',
        CardJ <$ char 'J',
        CardT <$ char 'T'
      ]

getHandTypeWithJoker :: (Ord a) => a -> Hand a -> HType
getHandTypeWithJoker joker h@(Hand c1 c2 c3 c4 c5) =
  case (handType, jokerCount) of
    (High, 1) -> OnePair
    (OnePair, 1) -> ThreeKind
    (OnePair, 2) -> ThreeKind
    (TwoPair, 1) -> FullHouse
    (TwoPair, 2) -> FourKind
    (ThreeKind, 1) -> FourKind
    (ThreeKind, 3) -> FourKind
    (FourKind, 1) -> FiveKind
    (FourKind, 4) -> FiveKind
    (FullHouse, 2) -> FiveKind
    (FullHouse, 3) -> FiveKind
    _ -> handType
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

getTotalWin :: (Num a, Enum a, Ord b) => [(b, a)] -> a
getTotalWin input = L.sum $ L.zipWith getValue [1 ..] (L.sortOn fst input)
  where
    getValue pos (_, bid) = pos * bid

part1 :: IO Integer
part1 = do
  inputData <- readFile "./data/day7.txt"
  return $ case traverse (parseString handWithBid mempty) (lines inputData) of
       Success parsed -> getTotalWin parsed
       Failure err -> error $ show err
part2 :: IO Integer
part2 = do
  inputData <- readFile "./data/day7.txt"
  return $ case traverse (parseString handWithBid mempty) (lines inputData) of
       Success parsed -> getTotalWin (coerce parsed :: [(JkHand JkCard, Integer)])
       Failure err -> error $ show err
