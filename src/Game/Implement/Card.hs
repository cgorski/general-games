{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card
  where

import qualified Data.Set as DS
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.List (nub, maximumBy, minimumBy)

class (Enum c, Eq c, Ord c, Bounded c) => Card c where
  fullDeck :: [c]
  dedupe :: [c] -> [c]
  draw :: Int -> [c] -> Maybe ([c],[c])
  fullDeck = [minBound .. maxBound]
  dedupe l = nub l
  draw n l
    | n > (length l) = Nothing
    | otherwise = Just (drop n l, take n l)

class (Card c) => OrderedCard c o where
  highestCardBy :: o -> [c] -> c
  lowestCardBy :: o -> [c] -> c
  compareCardBy :: o -> c -> c -> Ordering
  highestCardBy o cl = maximumBy (compareCardBy o) cl
  lowestCardBy o cl = minimumBy (compareCardBy o) cl

