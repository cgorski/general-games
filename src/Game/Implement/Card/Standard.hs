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

ranks = [minBound .. maxBound] :: [Rank]
nRanks = length ranks 

data Suit =
  Clubs |
  Diamonds |
  Hearts |
  Spades
  deriving (Show, Enum, Eq, Ord, Bounded)

suits = [minBound .. maxBound] :: [Suit]
nSuits = length suits

instance Card PlayingCard where

data PlayingCard = PlayingCard Rank Suit deriving (Eq, Ord, Bounded)
data Value = RankValue | SuitValue deriving (Eq)

instance Enum PlayingCard where
  fromEnum (PlayingCard r s) =
      (fromEnum r)+((fromEnum s)*nRanks)
  toEnum n =
    let r = n `mod` nRanks
        s = n `mod` 4
    in
      (PlayingCard (toEnum r) (toEnum s))

instance Show PlayingCard where
  show (PlayingCard r s) = (show r) ++ " of " ++ (show s)

instance ValuedCard PlayingCard Rank where
  toValue (PlayingCard r _) = r

instance ValuedCard PlayingCard Suit where
  toValue (PlayingCard _ s) = s

toRank :: PlayingCard -> Rank
toRank (PlayingCard r _) = r

toRankLst :: [PlayingCard] -> [Rank]
toRankLst cl = map toRank cl

toSuit :: PlayingCard -> Suit
toSuit (PlayingCard _ s) = s






