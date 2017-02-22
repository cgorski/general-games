{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card
  where

import qualified Data.Set as DS
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.List (nub)

data ExampleCardType =
  Card1 
  | Card2
  | Card3
  | Card4
  deriving (Enum, Eq, Ord, Bounded)

data ExampleValueType = ValuePair Int Char deriving (Show, Eq)

data ExampleOrderingType = IntOrder | CharOrder

  
class (Enum c, Eq c, Ord c, Bounded c) => Card c where

class (Card c) => ValuedCard c v where
  showCard :: v -> c -> String
  toValue :: c -> v

class (Card c) => OrderedCard c o where
  compareCardBy :: o -> c -> c -> Ordering


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
  showCard v c = "foo"
  toValue Card1 = ValuePair 1 'z'
  toValue Card2 = ValuePair 2 'y'
  toValue Card3 = ValuePair 3 'x'
  toValue Card4 = ValuePair 4 'w'

class Card c => CardCollection c where
  fullDeck :: [c]
  dedupe :: [c] -> [c]
--  draw :: Int -> [c] -> ([c],[c])
  fullDeck = [minBound .. maxBound]
  dedupe l = nub l

instance CardCollection ExampleCardType where




deck :: [ExampleCardType]
deck = fullDeck

deck2 :: [ExampleValueType]
deck2 = map toValue deck



