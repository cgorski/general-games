{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card
  where

import qualified Data.Set as DS
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.List (nub)
  
class (Enum c, Eq c, Ord c, Bounded c) => Card c where

class (Card c) => ValuedCard c v where
  toValue :: c -> v

class (Card c) => OrderedCard c o where
  compareCardBy :: o -> c -> c -> Ordering

class Card c => CardCollection c where
  fullDeck :: [c]
  dedupe :: [c] -> [c]
  draw :: Int -> [c] -> Maybe ([c],[c])
  fullDeck = [minBound .. maxBound]
  dedupe l = nub l
  draw n l
    | n > (length l) = Nothing
    | otherwise = Just (drop n l, take n l)

