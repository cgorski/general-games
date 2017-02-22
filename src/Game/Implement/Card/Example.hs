{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card.Example
  where

import Game.Implement.Card

data ExampleCardType =
  Card1 
  | Card2
  | Card3
  | Card4
  deriving (Enum, Eq, Ord, Bounded)

data ExampleValueType = ValuePair Int Char deriving (Show, Eq)

data ExampleOrderingType = IntOrder | CharOrder

instance Card ExampleCardType where

instance OrderedCard ExampleCardType ExampleOrderingType where
  compareCardBy IntOrder c1 c2 =
    let
      ValuePair n1 _ = toValue c1
      ValuePair n2 _ = toValue c2
    in
      n1 `compare` n2
  compareCardBy CharOrder c1 c2 =
    let
      ValuePair _ m1 = toValue c1
      ValuePair _ m2 = toValue c2
    in
      m1 `compare` m2
      
instance ValuedCard ExampleCardType ExampleValueType where
  toValue Card1 = ValuePair 1 'z'
  toValue Card2 = ValuePair 2 'y'
  toValue Card3 = ValuePair 3 'x'
  toValue Card4 = ValuePair 4 'w'

instance CardCollection ExampleCardType where




deck :: [ExampleCardType]
deck = fullDeck

deck2 :: [ExampleValueType]
deck2 = map toValue deck

comp = compareCardBy CharOrder (deck !! 1) (deck !! 2)



