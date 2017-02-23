{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card.Standard.Poker
  where

import Game.Implement.Card
import Game.Implement.Card.Standard
import Data.List (maximumBy)

data Order = AceHighRankOrder | AceLowRankOrder | SuitOrder deriving (Eq)
data ValueType = RankValueType | SuitValueType

instance OrderedCard PlayingCard Order where
  compareCardBy AceHighRankOrder (PlayingCard Ace _) (PlayingCard _ _) = LT
  compareCardBy AceHighRankOrder (PlayingCard _ _) (PlayingCard Ace _) = GT
  compareCardBy AceHighRankOrder (PlayingCard r1 s1) (PlayingCard r2 s2) =
    if r1 == r2
    then EQ
    else r2 `compare` r1

  compareCardBy AceLowRankOrder (PlayingCard Ace s1) (PlayingCard r2 s2) = GT
  compareCardBy AceLowRankOrder (PlayingCard r1 s1) (PlayingCard Ace s2) = LT
  compareCardBy AceLowRankOrder (PlayingCard r1 s1) (PlayingCard r2 s2) =
    if r1 == r2
    then EQ
    else r2 `compare` r1
    
  compareCardBy SuitOrder (PlayingCard _ s1) (PlayingCard _ s2) = s1 `compare` s2


instance OrderedValuedCard PlayingCard Order ValueType where
  toOrderedValue AceLowRankOrder RankValueType (PlayingCard r _) = (fromEnum r) + 1
  toOrderedValue AceHighRankOrder RankValueType (PlayingCard r _) =
    case r of 
      Ace -> 14
      r -> (fromEnum r) + 1
  toOrderedValue _ SuitValueType (PlayingCard _ s) = fromEnum s
