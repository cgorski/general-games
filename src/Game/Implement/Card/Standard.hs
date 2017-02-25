{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card.Standard
  where

import Game.Implement.Card

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







