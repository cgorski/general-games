{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card
  where

import Data.List (nub, maximumBy, minimumBy, sortBy)

class (Enum c, Eq c, Ord c, Bounded c) => Card c where
  fullDeck :: [c]
  dedupe :: [c] -> [c]
  draw :: Int -> [c] -> Maybe ([c],[c])
  fullDeck = [minBound .. maxBound]
  dedupe l = nub l
  draw n l
    | n > (length l) = Nothing
    | otherwise = Just (drop n l, take n l)

class (Card c) => ValuedCard c v where
  toValue :: c -> v
  toValueLst :: [c] -> [v]
  toValueLst l = map toValue l

class (Card c) => OrderedCard c o where
  highestCardBy :: o -> [c] -> c
  lowestCardBy :: o -> [c] -> c
  compareCardBy :: o -> c -> c -> Ordering
  sortCardsBy :: o -> [c] -> [c]
  highestCardBy o cl = maximumBy (compareCardBy o) cl
  lowestCardBy o cl = minimumBy (compareCardBy o) cl
  sortCardsBy o cl = sortBy (compareCardBy o) cl

class (OrderedCard c o) => OrderedValuedCard c o vt where
  toOrderedValue :: o -> vt -> c -> Int


