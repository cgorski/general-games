{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card
  where

import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import Data.List (nub, maximumBy, minimumBy, sortBy, foldl1')

class (Enum c, Eq c, Ord c, Bounded c) => Card c where
  fullDeck :: [c]
  dedupe :: [c] -> [c]
  draw :: [Int] -> [c] -> Maybe ([[c]],[c])
  shuffle :: MonadRandom m => [c] -> m [c]
  fullDeck = [minBound .. maxBound]
  dedupe l = nub l
  shuffle deck = shuffleM deck
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


