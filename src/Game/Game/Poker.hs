{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Game.Poker
  where

import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker

import Data.List (tails,nub,find) --, sortBy, nub, find)
import Data.Maybe (isJust, fromJust)


type RankHand = [PlayingCard] 
type KickerHand = [PlayingCard]
data RankKicker = RankHand KickerHand deriving(Eq,Show)

data AceRank = AceHigh | AceLow deriving (Eq, Show)

orderOfAceRank :: AceRank -> Order
orderOfAceRank AceHigh = AceHighRankOrder
orderOfAceRank AceLow = AceLowRankOrder

data PokerHandType =
  HighCard 
  | Pair 
  | TwoPair 
  | ThreeOfAKind 
  | Straight AceRank
  | Flush 
  | FullHouse 
  | FourOfAKind 
  | StraightFlush AceRank
  | RoyalFlush 
  deriving(Eq,Show)

data PokerHandSplit = PokerHandType RankKicker deriving(Eq,Show)
data PokerHand = PokerHand PokerHandType [PlayingCard] deriving(Eq,Show)

-- mkBestHand :: S.Hand -> Maybe PokerHand
-- mkBestHand hand
--   | isPokerHandSize hand = Nothing
--   | otherwise = mkHighCard hand


isSameSuit :: [PlayingCard] -> Bool
isSameSuit hand =
  let
    ff (Just c0) (Just c1) =
      if (toSuit c0) == (toSuit c1)
      then Just c1
      else Nothing
    ff _ _ = Nothing
  in
    case foldl1 ff $ map (\x -> Just x) hand of
      Nothing -> False
      Just _ -> True

hasConsecutiveRanks :: Order -> [PlayingCard] -> Bool
hasConsecutiveRanks order hand =
  let handlst = map (\x -> Just x) $ sortCardsBy order hand
      ff (Just c0) (Just c1) =
        case (toOrderedValue order RankValueType c0)-(toOrderedValue order RankValueType c1) of
          1 -> Just c1
          _ -> Nothing
      ff _ _ = Nothing
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
    nub $ map countel uniquelst
  
hasNOfRank :: Int -> [PlayingCard] -> Bool
hasNOfRank i hand =
  case (find (\(_,n) -> i == n) (nOfRank hand)) of
    Just _ -> True
    Nothing -> False

hasNumNOfRank :: Int -> Int -> [PlayingCard] -> Bool
hasNumNOfRank i num hand =
  if (length (filter  (\(_,n) -> i == n) (nOfRank hand))) == num
  then True
  else False

mkHighCard :: [PlayingCard] -> Maybe PokerHand
mkHighCard hand
  | isPokerHandSize hand =
      if (not $ isPair hand)
         && (not $ isTwoPair hand)
         && (not $ isThreeOfAKind hand)
         && (not $ isStraight hand)
         && (not $ isFlush hand)
         && (not $ isFullHouse hand)
         && (not $ isFourOfAKind hand)
         && (not $ isStraightFlush hand)
         && (not $ isRoyalFlush hand)
      then Just (PokerHand HighCard hand)
      else Nothing
  | otherwise = Nothing
      
isHighCard :: [PlayingCard] -> Bool
isHighCard hand
  | isJust $ mkHighCard hand = True
  | otherwise = False


mkPair :: [PlayingCard] -> Maybe PokerHand
mkPair hand
  | isPokerHandSize hand =
      if (hasNumNOfRank 2 1 hand)
         && (not $ isFullHouse hand)
      then Just (PokerHand Pair hand)
      else Nothing
  | otherwise = Nothing

isPair :: [PlayingCard] -> Bool
isPair hand
  | isJust $ mkPair hand = True
  | otherwise = False

mkTwoPair :: [PlayingCard] -> Maybe PokerHand
mkTwoPair hand
  | isPokerHandSize hand =
      if (hasNumNOfRank 2 2 hand)
         && (not $ isFullHouse hand) 
      then Just (PokerHand TwoPair hand)
      else Nothing
  | otherwise = Nothing

isTwoPair :: [PlayingCard] -> Bool
isTwoPair hand
  | isJust $ mkTwoPair hand = True
  | otherwise = False


mkThreeOfAKind :: [PlayingCard] -> Maybe PokerHand
mkThreeOfAKind hand
  | isPokerHandSize hand =
      if (hasNOfRank 3 hand)
         && (not $ isFullHouse hand)
      then Just (PokerHand ThreeOfAKind hand)
      else Nothing
  | otherwise = Nothing

isThreeOfAKind :: [PlayingCard] -> Bool
isThreeOfAKind hand
  | isJust $ mkThreeOfAKind hand = True
  | otherwise = False

mkConsecutiveRanks :: [PlayingCard] -> Maybe ([PlayingCard], AceRank)
mkConsecutiveRanks hand =
  let consecHigh h = (hasConsecutiveRanks AceHighRankOrder h)
      consecLow h = (hasConsecutiveRanks AceLowRankOrder h)
      f h2
        | consecHigh h2 = Just (sortCardsBy AceHighRankOrder h2, AceHigh)
        | consecLow h2 = Just (sortCardsBy AceLowRankOrder h2, AceLow)
        | otherwise = Nothing
  in f hand
        
        

mkStraight :: [PlayingCard] -> Maybe PokerHand
mkStraight hand
  | isPokerHandSize hand =
      let consecRanks  = mkConsecutiveRanks hand
          isConsecRanks = isJust consecRanks in
        if isConsecRanks
        && (not $ isRoyalFlush hand)
        && (not $ isStraightFlush hand)
      then Just (PokerHand (Straight $ snd $ fromJust consecRanks) (fst $ fromJust consecRanks))
      else Nothing
  | otherwise = Nothing

isStraight :: [PlayingCard] -> Bool
isStraight hand
  | isJust $ mkStraight hand = True
  | otherwise = False

mkFlush :: [PlayingCard] -> Maybe PokerHand
mkFlush hand
  | isPokerHandSize hand =
      if (isSameSuit hand)
         && (not $ isRoyalFlush hand)
         && (not $ isStraightFlush hand) 
      then Just (PokerHand Flush (sortCardsBy AceHighRankOrder hand))
      else Nothing
  | otherwise = Nothing

isFlush :: [PlayingCard] -> Bool
isFlush hand
  | isJust $ mkFlush hand = True
  | otherwise = False

mkFullHouse :: [PlayingCard] -> Maybe PokerHand
mkFullHouse hand
  | isPokerHandSize hand =
      if (hasNOfRank 3 hand)
         && (hasNOfRank 2 hand)
      then Just (PokerHand FullHouse hand)
      else Nothing
  | otherwise = Nothing

isFullHouse :: [PlayingCard] -> Bool
isFullHouse hand
  | isJust $ mkFullHouse hand = True
  | otherwise = False
    
mkFourOfAKind :: [PlayingCard] -> Maybe PokerHand
mkFourOfAKind hand
  | isPokerHandSize hand = 
      if (hasNOfRank 4 hand)
      then Just (PokerHand FourOfAKind hand)
      else Nothing
  | otherwise = Nothing

isFourOfAKind :: [PlayingCard] -> Bool
isFourOfAKind hand
  | isJust $ mkFourOfAKind hand = True
  | otherwise = False
                
mkStraightFlush :: [PlayingCard] -> Maybe PokerHand
mkStraightFlush hand
  | isPokerHandSize hand =
      let consecRanks  = mkConsecutiveRanks hand
          isConsecRanks = isJust consecRanks in
        if isConsecRanks
        && (isSameSuit hand)
        && (not $ isRoyalFlush hand)
      then Just (PokerHand (Straight $ snd $ fromJust consecRanks) (fst $ fromJust consecRanks))
      else Nothing
  | otherwise = Nothing

isStraightFlush :: [PlayingCard] -> Bool
isStraightFlush hand
  | isJust $ mkStraightFlush hand = True
  | otherwise = False

mkRoyalFlush :: [PlayingCard] -> Maybe PokerHand
mkRoyalFlush hand
  | isPokerHandSize hand =
      if (isSameSuit hand)
      then
        let
          slst :: [PlayingCard] = sortCardsBy AceHighRankOrder hand
          rlst = toValueLst slst
        in
          if (rlst == [Ace, King, Queen, Jack, Ten])
          then Just (PokerHand RoyalFlush hand)
          else Nothing
      else Nothing
  | otherwise = Nothing


isRoyalFlush :: [PlayingCard] -> Bool
isRoyalFlush hand
  | isJust $ mkRoyalFlush hand = True
  | otherwise = False

isPokerHandSize :: [PlayingCard] -> Bool
isPokerHandSize hand 
   | (length hand) == 5 = True
   | otherwise = False

choose :: Ord r => Int -> [r] -> [[r]]
choose 0 _ = [[]]
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

allTwoPair :: [[PlayingCard]]
allTwoPair = [x | x <- allPossibleHands, isTwoPair x]

allPair :: [[PlayingCard]]
allPair = [x | x <- allPossibleHands, isPair x]

allHighCard :: [[PlayingCard]]
allHighCard = [x | x <- allPossibleHands, isHighCard x]


