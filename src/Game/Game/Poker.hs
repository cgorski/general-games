{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module      : Game.Implement.Card
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Game.Poker module provides operations for five card poker.
module Game.Game.Poker
  (
    AceRank (AceHigh, AceLow)
  , PokerHand
  
  , cardsOfPokerHand
  
  , allPossibleHands

  , allRoyalFlush
  , allStraightFlush
  , allFourOfAKind
  , allFullHouse
  , allFlush
  , allStraight
  , allThreeOfAKind
  , allTwoPair
  , allPair
  , allHighCard
  
  , isRoyalFlush
  , isStraightFlush
  , isFourOfAKind
  , isFullHouse
  , isFlush
  , isStraight
  , isThreeOfAKind
  , isTwoPair
  , isPair
  , isHighCard

  , mkHand
  , mkRoyalFlush
  , mkStraightFlush
  , mkFourOfAKind
  , mkFullHouse
  , mkFlush
  , mkStraight
  , mkThreeOfAKind
  , mkTwoPair
  , mkPair
  , mkHighCard
  
  , randomStraight
  , randomFlush
  , randomFourOfAKind
  , randomStraightFlush
  , randomRoyalFlush
  
  , mkConsecutiveRanks
  )

   
  where


import Control.Monad.Loops
import Control.Monad.Random
import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker
import Data.List (tails,nub,find) 
import Data.Maybe (isJust, fromJust, catMaybes)
import System.Random.Shuffle (shuffleM)


randomAceRank :: MonadRandom m => m AceRank
randomAceRank =
  let
    minB = minBound :: AceRank
    maxB = maxBound :: AceRank in
    do
      (randomn :: Int) <- getRandomR(fromEnum minB, fromEnum maxB);
      return $ toEnum randomn
orderOfAceRank :: AceRank -> Order
orderOfAceRank AceHigh = AceHighRankOrder
orderOfAceRank AceLow = AceLowRankOrder

-- |
-- Indicates if a poker hand uses the Ace as a high card or a low card.
--
-- >>>
data AceRank = AceHigh | AceLow deriving (Eq, Show, Enum, Bounded)

cardsOfPokerHand (PokerHand _ h) = h

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

data PokerHand = PokerHand PokerHandType [PlayingCard] deriving(Eq,Show)



randomStraight :: RandomGen g => Rand g PokerHand
randomStraight =
  let
    mkRanklst :: Int -> [Rank]
    mkRanklst n = map (\m -> toEnum ((m+n) `mod` 13) ) [0..4] 
    mergelst r s = return(PlayingCard r s)
    l = do
      startRank :: Int <- getRandomR(0,9)

      ranklst <- return (mkRanklst startRank)
      suitlst :: [Suit] <- replicateM 5 randomSuit
      cardset <- zipWithM mergelst ranklst suitlst
      return cardset
  in
    do
      hand <- iterateUntil (\h -> (not $ isStraightFlush h) && (not $ isRoyalFlush h)) l
      aceRank <- return (if (toRank $  hand !! 0) == Ace then AceLow else AceHigh)
      shuffledHand <- shuffle hand
      return $ PokerHand (Straight aceRank) shuffledHand

randomFlush :: RandomGen g => Rand g PokerHand
randomFlush =
  let
    l = do
      numLst <- uniqueNumList 5 0 12
      rankLst :: [Rank] <- return $ map (\n -> toEnum n) $ fromJust $ numLst
      randSuit <- randomSuit
      suitLst :: [Suit] <- return $ replicate 5 randSuit
      cardset <- zipWithM (\r s -> return(PlayingCard r s)) rankLst suitLst 
      return cardset
  in
    do
      hand <- iterateUntil (\h -> (not $ isRoyalFlush h) && (not $ isStraightFlush h)) l
      return $ PokerHand Flush hand
      
randomFourOfAKind :: RandomGen g => Rand g PokerHand
randomFourOfAKind =
  do
    randRank4 <- randomRank
    randRank <- iterateUntil (\r -> r /= randRank4) randomRank
    randRanks <- return $ randRank:(replicate 4 randRank4)
    randSuit <- randomSuit
    randSuits <- return [randSuit, Clubs, Diamonds, Hearts, Spades]
    mergedLst <- zipWithM (\r s -> return(PlayingCard r s)) randRanks randSuits
    shuffleSet <- shuffle mergedLst
    return $ PokerHand FourOfAKind $ shuffleSet

randomStraightFlush :: RandomGen g => Rand g PokerHand
randomStraightFlush =
  let
    mkRanklst :: Int -> [Rank]
    mkRanklst n = map (\m -> toEnum ((m+n) `mod` 13) ) [0..4] 
    mergelst r s = return(PlayingCard r s)
    l = do 
      startRank :: Int <- getRandomR(0,9)
      ranklst <- return (mkRanklst startRank)
      randSuit <- randomSuit
      suitlst :: [Suit] <- return (replicate 5 randSuit)
      cardset <- zipWithM mergelst ranklst suitlst
      return cardset 
  in
    do
      hand <- iterateUntil (\h -> (not $ isRoyalFlush h)) l
      aceRank <- return (if (toRank $  hand !! 0) == Ace then AceLow else AceHigh)
      shuffledHand <- shuffle hand
      return $ PokerHand (StraightFlush aceRank) shuffledHand

randomRoyalFlush :: RandomGen g => Rand g PokerHand
randomRoyalFlush =
  let
    mkRanklst :: [Rank]
    mkRanklst = Ace : (map (\m -> toEnum m) [9..12])
    mergelst r s = return(PlayingCard r s) in
    do 
      startRank :: Int <- getRandomR(0,9)
      randSuit <- randomSuit
      suitlst :: [Suit] <- return (replicate 5 randSuit)
      cardset <- zipWithM mergelst mkRanklst suitlst
      shuffledHand <- shuffle cardset
      return $ PokerHand RoyalFlush shuffledHand

mkHand :: [PlayingCard] -> Maybe PokerHand
mkHand hand =
  let checks =
        [mkHighCard
        ,mkPair
        ,mkTwoPair
        ,mkThreeOfAKind
        ,mkStraight
        ,mkFlush
        ,mkFullHouse
        ,mkFourOfAKind
        ,mkStraightFlush
        ,mkRoyalFlush]
      cat = catMaybes $ map (\f -> f hand) checks
  in 
    if length cat == 0
    then Nothing
    else Just $ cat !! 0

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
  | isValidPokerHand hand =
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
  | isValidPokerHand hand =
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
  | isValidPokerHand hand =
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
  | isValidPokerHand hand =
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
  | isValidPokerHand hand =
      let consecRanks  = mkConsecutiveRanks hand
          isConsecRanks = isJust consecRanks in
        if isConsecRanks
        && (not $ isRoyalFlush hand)
        && (not $ isStraightFlush hand)
      then Just (PokerHand (Straight $ snd $ fromJust consecRanks) hand)
      else Nothing
  | otherwise = Nothing

isStraight :: [PlayingCard] -> Bool
isStraight hand
  | isJust $ mkStraight hand = True
  | otherwise = False

mkFlush :: [PlayingCard] -> Maybe PokerHand
mkFlush hand
  | isValidPokerHand hand =
      if (isSameSuit hand)
         && (not $ isRoyalFlush hand)
         && (not $ isStraightFlush hand) 
      then Just (PokerHand Flush hand)
      else Nothing
  | otherwise = Nothing

isFlush :: [PlayingCard] -> Bool
isFlush hand
  | isJust $ mkFlush hand = True
  | otherwise = False

mkFullHouse :: [PlayingCard] -> Maybe PokerHand
mkFullHouse hand
  | isValidPokerHand hand =
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
  | isValidPokerHand hand = 
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
  | isValidPokerHand hand =
      let consecRanks  = mkConsecutiveRanks hand
          isConsecRanks = isJust consecRanks in
        if isConsecRanks
        && (isSameSuit hand)
        && (not $ isRoyalFlush hand)
      then Just (PokerHand (Straight $ snd $ fromJust consecRanks) hand)
      else Nothing
  | otherwise = Nothing

isStraightFlush :: [PlayingCard] -> Bool
isStraightFlush hand
  | isJust $ mkStraightFlush hand = True
  | otherwise = False

mkRoyalFlush :: [PlayingCard] -> Maybe PokerHand
mkRoyalFlush hand 
  | isValidPokerHand hand =
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

isValidPokerHand :: [PlayingCard] -> Bool
isValidPokerHand hand 
   | ((length hand) == 5) && ((dedupe hand) == hand) = True
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


