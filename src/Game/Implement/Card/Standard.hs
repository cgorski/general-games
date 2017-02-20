module Game.Implement.Card.Standard
  where

import Data.Choose

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
  deriving (Eq, Ord, Bounded, Enum, Show)


data Card = Card Rank Suit 

instance Eq Card where
  (Card r s) == (Card r1 s1) = (r == r1) && (s == s1)

instance Ord Card where
  compare (Card r s) (Card r1 s1) = s `compare` s1


mkCard :: Rank -> Suit -> Card
mkCard r s = Card r s

instance Show Card where
  show (Card r s) = (show r) ++ " of " ++ (show s)

rankLst :: [Rank]
rankLst = [minBound .. maxBound]

suitLst :: [Suit]
suitLst = [minBound .. maxBound]

cardLst :: [Card]
cardLst = [Card r s | s <- suitLst, r <- rankLst]

numCards :: Int
numCards = length cardLst

class StandardCard a where
  toStandardCard :: a -> Card

class (Eq a, Ord a, StandardCard a) => OrderedCard a where

instance StandardCard Card where
  toStandardCard a = a




