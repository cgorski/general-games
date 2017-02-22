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
data Order = AceHighRankOrder | AceLowRankOrder | SuitOrder deriving (Eq)

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
  toValue (PlayingCard r s) = r

instance ValuedCard PlayingCard Suit where
  toValue (PlayingCard r s) = s

instance OrderedCard PlayingCard Order where
  compareCardBy AceHighRankOrder (PlayingCard Ace s1) (PlayingCard r2 s2) = GT
  compareCardBy AceHighRankOrder (PlayingCard r1 s1) (PlayingCard Ace s2) = LT
  compareCardBy AceHighRankOrder (PlayingCard r1 s1) (PlayingCard r2 s2) =
    if r1 == r2
    then EQ
    else r1 `compare` r2

  compareCardBy AceLowRankOrder (PlayingCard Ace s1) (PlayingCard r2 s2) = LT
  compareCardBy AceLowRankOrder (PlayingCard r1 s1) (PlayingCard Ace s2) = GT
  compareCardBy AceLowRankOrder (PlayingCard r1 s1) (PlayingCard r2 s2) =
    if r1 == r2
    then EQ
    else r1 `compare` r2
    
  compareCardBy SuitOrder (PlayingCard _ s1) (PlayingCard _ s2) = s1 `compare` s2



    

  
pdeck :: [PlayingCard]
pdeck = fullDeck





