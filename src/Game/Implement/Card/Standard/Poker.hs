{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card.Standard.Poker
  where

import Game.Implement.Card
import Game.Implement.Card.Standard
import Data.List (maximumBy)

data Order = AceHighRankOrder | AceLowRankOrder | SuitOrder deriving (Eq)

instance OrderedCard PlayingCard Order where
  highestCardBy o cl = maximumBy (compareCardBy o) cl
  compareCardBy AceHighRankOrder (PlayingCard Ace _) (PlayingCard _ _) = GT
  compareCardBy AceHighRankOrder (PlayingCard _ _) (PlayingCard Ace _) = LT
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
