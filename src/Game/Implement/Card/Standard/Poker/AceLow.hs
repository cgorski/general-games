module Game.Implement.Card.Standard.Poker.AceLow
  where

import qualified Game.Implement.Card.Standard as C

newtype Rank = Rank { getStandardRank :: C.Rank } deriving (Show)
newtype Suit = Suit { getStandardSuit :: C.Suit } deriving (Show)

instance Bounded Rank where
  minBound = Rank C.Ace
  maxBound = Rank C.King

instance Enum Rank where
  toEnum 1 = Rank C.Ace
  toEnum 2 = Rank C.Two
  toEnum 3 = Rank C.Three
  toEnum 4 = Rank C.Four
  toEnum 5 = Rank C.Five
  toEnum 6 = Rank C.Six
  toEnum 7 = Rank C.Seven
  toEnum 8 = Rank C.Eight
  toEnum 9 = Rank C.Nine
  toEnum 10 = Rank C.Ten
  toEnum 11 = Rank C.Jack
  toEnum 12 = Rank C.Queen
  toEnum 13 = Rank C.King

  fromEnum (Rank C.Ace) = 1
  fromEnum (Rank C.Two) = 2
  fromEnum (Rank C.Three) = 3
  fromEnum (Rank C.Four) = 4
  fromEnum (Rank C.Five) = 5
  fromEnum (Rank C.Six) = 6
  fromEnum (Rank C.Seven) = 7
  fromEnum (Rank C.Eight) = 8
  fromEnum (Rank C.Nine) = 9
  fromEnum (Rank C.Ten) = 10
  fromEnum (Rank C.Jack) = 11
  fromEnum (Rank C.Queen) = 12
  fromEnum (Rank C.King) = 13

instance Eq Rank where
  a == b = (fromEnum a) == (fromEnum b)

instance Ord Rank where
  compare a b = (fromEnum b) `compare` (fromEnum a)


instance Bounded Suit where
  minBound = Suit C.Clubs
  maxBound = Suit C.Spades

instance Enum Suit where
  toEnum 0 = Suit C.Clubs
  toEnum 1 = Suit C.Diamonds
  toEnum 2 = Suit C.Hearts
  toEnum 3 = Suit C.Spades

  fromEnum (Suit C.Clubs) = 0
  fromEnum (Suit C.Diamonds) = 1
  fromEnum (Suit C.Hearts) = 2
  fromEnum (Suit C.Spades) = 3

instance Eq Suit where
  a == b = (fromEnum a) == (fromEnum b)

instance Ord Suit where
  compare a b = (fromEnum a) `compare` (fromEnum b)
  
data Card = Card Rank Suit

instance Eq Card where
  (Card r s) == (Card r1 s1) = (r == r1) && (s == s1)

instance Ord Card where
  compare (Card r s) (Card r1 s1)
    | s == s1 = r `compare` r1
    | otherwise = s `compare` s1


mkCard :: Rank -> Suit -> Card
mkCard r s = Card r s

instance Show Card where
  show (Card r s) = (show r) ++ " of " ++ (show s)

instance C.StandardCard Card where
  toStandardCard = getStandardCard
  toRank c =
    let (C.Card r _ ) = getStandardCard c
        in r
  toSuit c =
    let (C.Card _ s ) = getStandardCard c
        in s 

instance C.OrderedCard Card where
  compareRank (Card r0 _) (Card r1 _) = r0 `compare` r1
  compareSuit (Card _ s0) (Card _ s1) = s0 `compare` s1

rankLst :: [Rank]
rankLst = [minBound .. maxBound]

suitLst :: [Suit]
suitLst = [minBound .. maxBound]

cardLst :: [Card]
cardLst = [Card r s | s <- suitLst, r <- rankLst]

numCards :: Int
numCards = length cardLst

getStandardCard :: Card -> C.Card
getStandardCard (Card r s) = C.Card (getStandardRank r) (getStandardSuit s)

fromStandardCard :: C.StandardCard c => c -> Card
fromStandardCard c =
  let (C.Card cr cs) = C.toStandardCard c
      r = Rank cr
      s = Suit cs in
    Card r s 

fromStandardCardLst :: C.StandardCard c => [c] -> [Card]
fromStandardCardLst clst =
  map fromStandardCard clst

