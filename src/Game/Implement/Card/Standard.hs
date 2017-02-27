{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Implement.Card.Standard
  where

import Control.Monad.Random
import Game.Implement.Card
import System.Random.Shuffle(shuffleM)


data Rank =
  Ace |
  Two |
  Three |
  Four |
  Five |
  Six |
  Seven |
  Eight |
  Nine |
  Ten |
  Jack |
  Queen |
  King

  deriving (Show, Enum, Eq, Ord, Bounded)

randomRank :: RandomGen m => Rand m Rank
randomRank =
  let
    min = minBound :: Rank
    max = maxBound :: Rank in
    do
      (randomn :: Int) <- getRandomR(fromEnum min, fromEnum max);
      return $ toEnum randomn

randomRankR :: RandomGen m => Rank -> Rank -> Rand m Rank
randomRankR l u =
  let
    min = l :: Rank
    max = u :: Rank in
    do
      (randomn :: Int) <- getRandomR(fromEnum l, fromEnum u);
      return $ toEnum randomn

ranks :: [Rank]
ranks = [minBound .. maxBound]

nRanks :: Int
nRanks = length ranks 

data Suit =
  Clubs |
  Diamonds |
  Hearts |
  Spades
  deriving (Show, Enum, Eq, Ord, Bounded)

randomSuit :: RandomGen m => Rand m Suit
randomSuit =
  let
    min = minBound :: Suit
    max = maxBound :: Suit in
    do
      (randomn :: Int) <- getRandomR(fromEnum min, fromEnum max);
      return $ toEnum randomn

randomSuitR :: RandomGen m => Suit -> Suit -> Rand m Suit
randomSuitR l u =
  let
    min = l :: Suit
    max = u :: Suit in
    do
      (randomn :: Int) <- getRandomR(fromEnum l, fromEnum u);
      return $ toEnum randomn


uniqueNumList :: RandomGen g => Int -> Int -> Int -> Rand g (Maybe [Int])
uniqueNumList numToReturn n m  =
  if (numToReturn > ((m-n)+1)) || (numToReturn < 0)
  then return Nothing
  else
    do
      deck <- shuffleM [n..m]
      return $ Just $ take numToReturn deck

suits :: [Suit]
suits = [minBound .. maxBound]

nSuits :: Int
nSuits = length suits

instance Card PlayingCard where

data PlayingCard = PlayingCard Rank Suit deriving (Eq, Ord, Bounded)
data Value = RankValue | SuitValue deriving (Eq)

instance Enum PlayingCard where
  fromEnum (PlayingCard r s) =
      ((fromEnum s)*nRanks)+(fromEnum r)
  toEnum n =
    let r = n `mod` nRanks
        s = n `div` nRanks
    in
      (PlayingCard (toEnum r) (toEnum s))

instance Show PlayingCard where
  show (PlayingCard r s) = (show r) ++ " of " ++ (show s)

instance ValuedCard PlayingCard Rank where
  toValue (PlayingCard r _) = r

toRank :: PlayingCard -> Rank
toRank c = toValue c 

toRankLst :: [PlayingCard] -> [Rank]
toRankLst l = toValueLst l

instance ValuedCard PlayingCard Suit where
  toValue (PlayingCard _ s) = s

toSuit :: PlayingCard -> Suit
toSuit (PlayingCard _ s) = s










