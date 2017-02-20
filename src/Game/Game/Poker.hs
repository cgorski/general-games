module Game.Game.Poker
  where

import qualified Game.Implement.Card.Standard as S
import qualified Game.Implement.Card.Standard.Poker.AceHigh as AH
import qualified Game.Implement.Card.Standard.Poker.AceLow as AL
import qualified Data.Set as DS
import Data.List (tails, sortBy)
import Data.Maybe (isJust)



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
  deriving(Eq,Show)

mkBestHand :: S.Hand -> Maybe PokerHand
mkBestHand hand
  | isMinHandSize hand = Nothing
  | otherwise = mkHighCard hand

mkHighCard :: S.Hand -> Maybe PokerHand
mkHighCard hand
  | isMinHandSize hand = Nothing
  | otherwise = 
      let highCard = DS.singleton $ maximum hand
          kickers = DS.deleteMax hand in
        Just $ HighCard highCard kickers

isSameSuit :: [S.Card] -> Bool
isSameSuit lst =
  let
    ff (Just c0) (Just c1) =
      if ((S.toSuit c0) == (S.toSuit c1))
      then Just c1
      else Nothing
    ff Nothing _ = Nothing
  in
    case foldl1 ff $ map (\x -> Just x) lst of
      Nothing -> False
      Just _ -> True

mkRoyalFlush :: S.Hand -> Maybe PokerHand
mkRoyalFlush hand
  | (isMinHandSize hand) && (isSameSuit $ DS.toList hand) =
      let
        lst = DS.toList hand
        slst = sortHighToLow lst
        rlst = S.toRankLst slst
      in
        if (rlst == [S.Ace, S.King, S.Queen, S.Jack, S.Ten])
        then Just (RoyalFlush (DS.fromList slst))
        else Nothing
  | otherwise = Nothing

isRoyalFlush :: S.Hand -> Bool
isRoyalFlush hand
  | isJust $ mkRoyalFlush hand = True
  | otherwise = False

sortHighToLow :: [S.Card] -> [S.Card]
sortHighToLow lst = S.toStandardCardLst $ sortBy S.compareRank $ AH.fromStandardCardLst lst

standardLstToHand :: S.StandardCard s => [s] -> S.Hand
standardLstToHand lst = DS.fromList $ S.toStandardCardLst lst

isMinHandSize :: S.Hand -> Bool
isMinHandSize hand
  | (length hand) < 5 = False
  | otherwise = True

choose :: Ord r => Int -> [r] -> [[r]]
choose 0 lst = [[]]
choose n lst = do
  (x:xs) <- tails lst
  rest <- choose (n-1) xs
  return $ x : rest

allPossibleHands :: [S.Hand]
allPossibleHands = map DS.fromList $ choose 5 S.cardLst

allRoyalFlush :: [S.Hand]
allRoyalFlush = [x | x <- allPossibleHands, isRoyalFlush x]

