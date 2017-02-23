{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.Game.Poker
  where

import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker

import Data.List (tails) --, sortBy, nub, find)
import Data.Maybe (isJust)


type RankHand = [PlayingCard] 
type KickerHand = [PlayingCard]
data RankKicker = RankHand KickerHand deriving(Eq,Show)

data PokerHandType =
  HighCard 
  | Pair 
  | TwoPair 
  | ThreeOfAKind 
  | Straight 
  | Flush 
  | FullHouse 
  | FourOfAKind 
  | StraightFlush 
  | RoyalFlush 
  deriving(Eq,Show)

data PokerHandSplit = PokerHandType RankKicker deriving(Eq,Show)
data PokerHand = PokerHand PokerHandType [PlayingCard] deriving(Eq,Show)

-- mkBestHand :: S.Hand -> Maybe PokerHand
-- mkBestHand hand
--   | isMinHandSize hand = Nothing
--   | otherwise = mkHighCard hand

-- mkHighCard :: S.Hand -> Maybe PokerHand
-- mkHighCard hand
--   | isMinHandSize hand = Nothing
--   | otherwise = 
--       let highCard = maximum hand
--           kickers = DS.deleteMax hand in
--         Just $ HighCard highCard kickers

isSameSuit :: [PlayingCard] -> Bool
isSameSuit hand =
  let
    ff (Just c0) (Just c1) =
      if (toSuit c0) == (toSuit c1)
      then Just c1
      else Nothing
    ff Nothing _ = Nothing
  in
    case foldl1 ff $ map (\x -> Just x) hand of
      Nothing -> False
      Just _ -> True

hasConsecutiveRanks :: Order -> [PlayingCard] -> Bool
hasConsecutiveRanks order hand =
  let handlst = map (\x -> Just x) $ sortCardsBy order hand
      ff Nothing _ = Nothing
      ff (Just c0) (Just c1) =
        case (toOrderedValue order RankValueType c0)-(toOrderedValue order RankValueType c1) of
          1 -> Just c1
          _ -> Nothing
  in
    case foldl1 ff handlst of
      Nothing -> False
      _ -> True

-- nOfRank :: S.Hand -> [(S.Rank, Int)]
-- nOfRank hand =
--   let
--     lst = DS.toList hand
--     rlst = S.toRankLst lst
--     uniquelst = nub lst
--     countel :: S.Card -> (S.Rank, Int)
--     countel card = ((S.toRank card), length [x | x <- rlst, (S.toRank card)==x])
--   in
--     map countel uniquelst
  
-- hasNOfRank :: Int -> S.Hand -> Bool
-- hasNOfRank i hand =
--   case (find (\(_,n) -> i == n) (DS.fromList $ nOfRank hand)) of
--     Just _ -> True
--     Nothing -> False
    
-- mkFourOfAKind :: S.Hand -> Maybe PokerHand
-- mkFourOfAKind hand
--   | (isMinHandSize hand)
--     && (hasNOfRank 4 hand) = Just (FourOfAKind hand)
--   | otherwise = Nothing

-- isFourOfAKind :: S.Hand -> Bool
-- isFourOfAKind hand
--   | isJust $ mkFourOfAKind hand = True
--   | otherwise = False
                
mkStraightFlush :: [PlayingCard] -> Maybe PokerHand
mkStraightFlush hand
  | (isMinHandSize hand) && (isSameSuit hand)
    && ((hasConsecutiveRanks AceHighRankOrder hand)
         || (hasConsecutiveRanks AceLowRankOrder hand))
    && (not $ isRoyalFlush hand) = Just (PokerHand StraightFlush (sortCardsBy AceHighRankOrder hand))
  | otherwise = Nothing

isStraightFlush :: [PlayingCard] -> Bool
isStraightFlush hand
  | isJust $ mkStraightFlush hand = True
  | otherwise = False

mkRoyalFlush :: [PlayingCard] -> Maybe PokerHand
mkRoyalFlush hand
  | (isMinHandSize hand) && (isSameSuit hand) =
      let
        slst :: [PlayingCard] = sortCardsBy AceHighRankOrder hand
        rlst = toValueLst slst
      in
        if (rlst == [Ace, King, Queen, Jack, Ten])
        then Just (PokerHand RoyalFlush slst)
        else Nothing
  | otherwise = Nothing


isRoyalFlush :: [PlayingCard] -> Bool
isRoyalFlush hand
  | isJust $ mkRoyalFlush hand = True
  | otherwise = False

-- sortHighToLow :: S.OrderedCard o => [o] -> [S.Card]
-- sortHighToLow lst = S.toStandardCardLst $ sortBy S.compareRank lst

-- standardLstToHand :: S.StandardCard s => [s] -> S.Hand
-- standardLstToHand lst = DS.fromList $ S.toStandardCardLst lst

isMinHandSize :: [PlayingCard] -> Bool
isMinHandSize hand 
   | (length hand) < 5 = False
   | otherwise = True

choose :: Ord r => Int -> [r] -> [[r]]
choose 0 lst = [[]]
choose n lst = do
  (x:xs) <- tails lst
  rest <- choose (n-1) xs
  return $ x : rest

allPossibleHands :: [[PlayingCard]]
allPossibleHands = choose 5 fullDeck

allRoyalFlush :: [[PlayingCard]]
allRoyalFlush = [x | x <- allPossibleHands, isRoyalFlush x]

allStraightFlush :: [[PlayingCard]]
allStraightFlush = [x | x <- allPossibleHands, isStraightFlush x]

-- allFourOfAKind :: [S.Hand]
-- allFourOfAKind = [x | x <- allPossibleHands, isFourOfAKind x]

