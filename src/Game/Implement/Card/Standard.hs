{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Game.Implement.Card.Standard
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Game.Poker module defines structures and operations for
-- a standard set of 52 playing cards.
module Game.Implement.Card.Standard
  (
    PlayingCard(..)
  , Rank(..)
  , Suit(..)
  , randomRank
  , randomRankR
  , randomSuit
  , randomSuitR
  , ranks
  , nRanks
  , toRank
  , toRankLst
  , suits
  , nSuits
  , toSuit
  , uniqueNumList
  , uniqueNumLists
  )
  where

import Control.Monad.Random
import Game.Implement.Card
import System.Random.Shuffle(shuffleM)

-- |
-- The rank of a standard playing card.
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

-- |
-- Returns a random standard playing card rank, with Ace low.
randomRank :: RandomGen m => Rand m Rank
randomRank =
  let
    minS = minBound :: Rank
    maxS = maxBound :: Rank in
    do
      (randomn :: Int) <- getRandomR(fromEnum minS, fromEnum maxS);
      return $ toEnum randomn

-- |
-- Returns a random standard playing card from a range, with Ace low.
randomRankR :: RandomGen m => Rank -> Rank -> Rand m Rank
randomRankR l u =
  do
    (randomn :: Int) <- getRandomR(fromEnum l, fromEnum u);
    return $ toEnum randomn

-- |
-- Returns all standard playing card ranks, with Ace low.
ranks :: [Rank]
ranks = [minBound .. maxBound]

-- |
-- Returns the number of standard playing card ranksu.
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
    minS = minBound :: Suit
    maxS = maxBound :: Suit in
    do
      (randomn :: Int) <- getRandomR(fromEnum minS, fromEnum maxS);
      return $ toEnum randomn

randomSuitR :: RandomGen m => Suit -> Suit -> Rand m Suit
randomSuitR l u =
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

uniqueNumLists :: RandomGen g => [Int] -> Int -> Int -> Rand g (Maybe [[Int]])
uniqueNumLists numToReturn n m  =
  if ((sum numToReturn > ((m-n)+1)) || (length $ filter (\x -> x <= 0) numToReturn)>0)
  then return Nothing
  else
    let
      f _ [] out = reverse out
      f deck (r:rs) out = f (drop r deck) rs ((take r deck):out) in
    do
      deck <- shuffleM [n..m]
      return $ Just $ f deck numToReturn []


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










