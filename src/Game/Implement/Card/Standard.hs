{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Game.Implement.Card.Standard
  where

import qualified Data.Set as DS


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
  deriving (Show)

instance Bounded Rank where
  minBound = Ace
  maxBound = King

instance Enum Rank where 
  toEnum 0 = Ace
  toEnum 1 = Two
  toEnum 2 = Three
  toEnum 3 = Four
  toEnum 4 = Five
  toEnum 5 = Six
  toEnum 6 = Seven
  toEnum 7 = Eight
  toEnum 8 = Nine
  toEnum 9 = Ten
  toEnum 10 = Jack
  toEnum 11 = Queen
  toEnum 12 = King

  fromEnum Ace = 0
  fromEnum Two = 1
  fromEnum Three = 2
  fromEnum Four = 3
  fromEnum Five = 4
  fromEnum Six = 5
  fromEnum Seven = 6
  fromEnum Eight = 7
  fromEnum Nine = 8
  fromEnum Ten = 9
  fromEnum Jack = 10
  fromEnum Queen = 11
  fromEnum King = 12

instance Eq Rank where
  a == b = (fromEnum a) == (fromEnum b)

instance Ord Rank where
  compare a b = (fromEnum a) `compare` (fromEnum b)



data Suit =
  Clubs |
  Diamonds |
  Hearts |
  Spades
  deriving (Eq, Show)


data Card r s = Card r s
data OrdCard r s = OrdCard r s




-- mkCard :: Card -> Rank -> Suit -> (Card Rank)
-- mkCard r s = Card r s

instance (Eq a, Eq b) => Eq (Card a b) where
  (Card r0 s0) == (Card r1 s1) = (r1 == r1) && (s1 == s1)

instance (Show a, Show b) => Show (Card a b) where
  show (Card r s) = (show r) ++ " of " ++ (show s)

-- rankLst :: [Rank]
-- rankLst = [minBound .. maxBound]

-- suitLst :: [Suit]
-- suitLst = [minBound .. maxBound]

-- cardLst :: [Card (Value Rank Suit)]
-- cardLst = [Card (Value r s) | s <- suitLst, r <- rankLst]

-- numCards :: Int
-- numCards = length cardLst

class (Show a, Eq a) => StandardRank a where

instance StandardRank Rank where

instance StandardSuit Suit where

class (Show a, Eq a) => StandardSuit a where  

class (StandardRank r, StandardSuit s) => StandardCard c r s where
  toRank :: c r s -> r
  toRankLst :: [c r s] -> [r]
  toSuit :: c r s -> s
  toSuitLst :: [c r s] -> [s]
  toRankLst lst = map toRank lst
  toSuitLst lst = map toSuit lst

class (Eq r, Eq s, Ord r, Ord s, StandardCard c r s) => OrderedCard c r s where
  compareRank :: c r s -> c r s -> Ordering
  compareSuit :: c r s -> c r s -> Ordering
  compareCard :: c r s -> c r s -> Ordering

instance StandardCard Card Rank Suit where
  toRank (Card r s) = r
  toSuit (Card r s) = s

type Hand r s = DS.Set (Card r s)


