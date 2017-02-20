module Game.Game.Poker
  where

import qualified Game.Implement.Card.Standard as S
import qualified Game.Implement.Card.Standard.Poker.AceHigh as AH
import qualified Game.Implement.Card.Standard.Poker.AceLow as AL
import qualified Data.Set as DS
import Data.List (tails)



type RankHand = S.Hand
type KickerHand = S.Hand

data PokerHand =
  HighCard RankHand KickerHand
  | Pair S.Hand
  | TwoPair S.Hand
  | ThreeOfAKind S.Hand
  | Straight S.Hand
  | Flush S.Hand
  | FullHouse S.Hand
  | FourOfAKind S.Hand
  | StraightFlush S.Hand
  | RoyalFlush S.Hand

mkBestHand :: S.Hand -> Maybe PokerHand
mkBestHand hand
  | isMinHand hand = Nothing
  | otherwise = mkHighCard hand

mkHighCard :: S.Hand -> Maybe PokerHand
mkHighCard hand
  | isMinHand hand = Nothing
  | otherwise = 
      let highCard = DS.singleton $ maximum hand
          kickers = DS.deleteMax hand in
        Just $ HighCard highCard kickers
        

isMinHand :: S.Hand -> Bool
isMinHand hand
  | (length hand) < 5 = False
  | otherwise = True

stdLst :: S.StandardCard s => [s] -> [S.Card]
stdLst lst = map S.toStandardCard lst


choose :: Ord r => Int -> [r] -> [[r]]
choose 0 lst = [[]]
choose n lst = do
  (x:xs) <- tails lst
  rest <- choose (n-1) xs
  return $ x : rest

allPossibleHands :: [S.Hand]
allPossibleHands = map DS.fromList $ choose 5 S.cardLst

