{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.Game.Poker
  where

import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker

import Data.List (tails,nub,find) --, sortBy, nub, find)
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

nOfRank :: [PlayingCard] -> [(Rank, Int)]
nOfRank hand =
  let
    rlst = toRankLst hand
    uniquelst = nub hand
    countel :: PlayingCard -> (Rank, Int)
    countel card = ((toRank card), length [x | x <- rlst, (toRank card)==x])
  in
    map countel uniquelst
  
hasNOfRank :: Int -> [PlayingCard] -> Bool
hasNOfRank i hand =
  case (find (\(_,n) -> i == n) (nOfRank hand)) of
    Just _ -> True
    Nothing -> False

mkThreeOfAKind :: [PlayingCard] -> Maybe PokerHand
mkThreeOfAKind hand
  | (isMinHandSize hand)
    && (hasNOfRank 3 hand)
    && (not $ isFullHouse hand) =
      Just (PokerHand ThreeOfAKind hand)
  | otherwise = Nothing

isThreeOfAKind :: [PlayingCard] -> Bool
isThreeOfAKind hand
  | isJust $ mkThreeOfAKind hand = True
  | otherwise = False


mkStraight :: [PlayingCard] -> Maybe PokerHand
mkStraight hand
  | (isMinHandSize hand)
    && ((hasConsecutiveRanks AceHighRankOrder hand)
         || (hasConsecutiveRanks AceLowRankOrder hand))
    && (not $ isRoyalFlush hand)
    && (not $ isStraightFlush hand) =
      Just (PokerHand Straight (sortCardsBy AceHighRankOrder hand))
  | otherwise = Nothing

isStraight :: [PlayingCard] -> Bool
isStraight hand
  | isJust $ mkStraight hand = True
  | otherwise = False

mkFlush :: [PlayingCard] -> Maybe PokerHand
mkFlush hand
  | (isMinHandSize hand) && (isSameSuit hand)
    && (not $ isRoyalFlush hand)
    && (not $ isStraightFlush hand) =
      Just (PokerHand Flush (sortCardsBy AceHighRankOrder hand))
  | otherwise = Nothing

isFlush :: [PlayingCard] -> Bool
isFlush hand
  | isJust $ mkFlush hand = True
  | otherwise = False


mkFullHouse :: [PlayingCard] -> Maybe PokerHand
mkFullHouse hand
  | (isMinHandSize hand)
    && (hasNOfRank 3 hand)
    && (hasNOfRank 2 hand) = Just (PokerHand FullHouse hand)
  | otherwise = Nothing

isFullHouse :: [PlayingCard] -> Bool
isFullHouse hand
  | isJust $ mkFullHouse hand = True
  | otherwise = False
    
mkFourOfAKind :: [PlayingCard] -> Maybe PokerHand
mkFourOfAKind hand
  | (isMinHandSize hand)
    && (hasNOfRank 4 hand) = Just (PokerHand FourOfAKind hand)
  | otherwise = Nothing

isFourOfAKind :: [PlayingCard] -> Bool
isFourOfAKind hand
  | isJust $ mkFourOfAKind hand = True
  | otherwise = False
                
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

allFourOfAKind :: [[PlayingCard]]
allFourOfAKind = [x | x <- allPossibleHands, isFourOfAKind x]

allFullHouse :: [[PlayingCard]]
allFullHouse = [x | x <- allPossibleHands, isFullHouse x]

allFlush :: [[PlayingCard]]
allFlush = [x | x <- allPossibleHands, isFlush x]

allStraight :: [[PlayingCard]]
allStraight = [x | x <- allPossibleHands, isStraight x]

allThreeOfAKind :: [[PlayingCard]]
allThreeOfAKind = [x | x <- allPossibleHands, isThreeOfAKind x]


