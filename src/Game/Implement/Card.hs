{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Game.Implement.Card
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Implement.Card module provides fundamental operations for a deck of cards.
module Game.Implement.Card
  (
    Card (..)
  , ValuedCard (..)
  , OrderedCard (..)
  , OrderedValuedCard (..)
  )
  where

import Control.Monad.Random
import Control.Monad.Loops
import System.Random.Shuffle (shuffleM)
import Data.List (nub, maximumBy, minimumBy, sortBy, foldl1')
import Data.Maybe (fromJust)

-- |
-- Represents a physical card with no order and no value.
-- Inherited Enum, Eq, Ord and Bounded typeclasses are used to
-- distingish cards for the purposes of manipulation within lists.
-- Game value functions are provided by other typeclasses.
class (Enum c, Eq c, Ord c, Bounded c) => Card c where
  -- |
  -- Return all cards in a list. Cards will appear at most once. Order is not guaranteed.
  --
  -- >>> fullDeck :: [PlayingCard]
  -- [Ace of Clubs,Two of Clubs,Three of Clubs,Four of Clubs,Five of Clubs,Six of Clubs,Seven of Clubs,Eight of Clubs,Nine of Clubs,Ten of Clubs,Jack of Clubs,Queen of Clubs,King of Clubs,Ace of Diamonds,Two of Diamonds,Three of Diamonds,Four of Diamonds,Five of Diamonds,Six of Diamonds,Seven of Diamonds,Eight of Diamonds,Nine of Diamonds,Ten of Diamonds,Jack of Diamonds,Queen of Diamonds,King of Diamonds,Ace of Hearts,Two of Hearts,Three of Hearts,Four of Hearts,Five of Hearts,Six of Hearts,Seven of Hearts,Eight of Hearts,Nine of Hearts,Ten of Hearts,Jack of Hearts,Queen of Hearts,King of Hearts,Ace of Spades,Two of Spades,Three of Spades,Four of Spades,Five of Spades,Six of Spades,Seven of Spades,Eight of Spades,Nine of Spades,Ten of Spades,Jack of Spades,Queen of Spades,King of Spades]
  fullDeck :: [c]
  dedupe :: [c] -> [c]
  draw :: [Int] -> [c] -> Maybe ([[c]],[c])
  shuffle :: RandomGen m => [c] -> Rand m [c]
  randomCard :: RandomGen m => Rand m c
  fullDeck = [minBound .. maxBound]
  dedupe l = nub l
  shuffle deck = shuffleM deck
  randomCard =
    let
      minB = minBound :: (Bounded c, Card c) => c
      maxB = maxBound :: (Bounded c, Card c) => c in
      do
        randomd <- getRandomR(fromEnum minB, fromEnum maxB)
        return $ toEnum randomd
      
  draw handSizeLst deck 
    | let
        total = (foldl1' (+) handSizeLst)
        anyNeg = (length (filter (\n -> n < 0) handSizeLst)) > 0
      in
        (total > (length deck)) || (total < 1) || anyNeg = Nothing
    | otherwise = let
        draw2 [] (houtput, doutput) = ((reverse houtput), doutput)
        draw2 (nToTake:hst) (handOutput, deckOutput) = let
          newHand = take nToTake deckOutput
          newDeck = drop nToTake deckOutput in
            draw2 hst (newHand:handOutput, newDeck)
        in Just (draw2 handSizeLst ([],deck))
-- |
-- Represents a playing card with a game value. For instance,
-- a standard playing card with a type representing
-- rank and suit.
class (Card c) => ValuedCard c v where
  toValue :: c -> v
  toValueLst :: [c] -> [v]
  toValueLst l = map toValue l

-- |
-- Orderings independent of a specific value
-- type of a Card.
class (Card c) => OrderedCard c o where
  highestCardBy :: o -> [c] -> c
  lowestCardBy :: o -> [c] -> c
  compareCardBy :: o -> c -> c -> Ordering
  sortCardsBy :: o -> [c] -> [c]
  highestCardBy o cl = maximumBy (compareCardBy o) cl
  lowestCardBy o cl = minimumBy (compareCardBy o) cl
  sortCardsBy o cl = sortBy (compareCardBy o) cl


class (OrderedCard c o) => OrderedValuedCard c o vt where
  -- |
  -- Return an Int based on a card, an ordering and a value type.
  toOrderedValue :: o -> vt -> c -> Int


