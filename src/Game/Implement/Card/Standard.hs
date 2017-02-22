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

data Suit =
  Clubs |
  Diamonds |
  Hearts |
  Spades
  deriving (Show, Enum, Eq, Ord, Bounded)

instance Card PlayingCard where

instance CardCollection PlayingCard where

data PlayingCard = PlayingCard Rank Suit deriving (Eq, Ord, Bounded)
data Value = RankValue | SuitValue deriving (Eq)
data Order = RankOrder | SuitOrder deriving (Eq)

instance Enum PlayingCard where
  fromEnum (PlayingCard r s) = (fromEnum r)+((fromEnum s)*13)
  toEnum n =
    let r = n `mod` 13
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
  compareCardBy RankOrder (PlayingCard r1 s1) (PlayingCard r2 s2) =
    if r1 == r2
    then s1 `compare` s1
    else r1 `compare` r2
  compareCardBy SuitOrder (PlayingCard _ s1) (PlayingCard _ s2) = s1 `compare` s2



    

  
pdeck :: [PlayingCard]
pdeck = fullDeck





