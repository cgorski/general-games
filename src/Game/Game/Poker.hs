module Game.Game.Poker
  where

import qualified Game.Implement.Card.Standard as S
import qualified Game.Implement.Card.Standard.Poker.AceHigh as AH
import qualified Game.Implement.Card.Standard.Poker.AceLow as AL
import qualified Data.Set as DS
import Data.List (tails)


type Hand = DS.Set S.Card
--type RankHand = Hand
--type KickerHand = Hand

data PokerHand =
  HighCard Hand
  | Pair Hand
  | TwoPair Hand
  | ThreeOfAKind Hand
  | StraightAL Hand
  | Flush Hand
  | FullHouse Hand
  | FourOfAKind Hand
  | StraightFlush Hand
  | RoyalFlush Hand

mkBestHand :: S.StandardCard s => [s] -> Hand
mkBestHand clst = DS.fromList $ stdLst clst

mkHighCard :: Hand -> Maybe PokerHand
mkHighCard c = Just $ HighCard $ DS.singleton $ S.toStandardCard $ maximum c

stdLst :: S.StandardCard s => [s] -> [S.Card]
stdLst lst = map S.toStandardCard lst

--allPossibleHands :: [Hand]
--allPossibleHands = map DS.fromList $ map (\h -> intsToCards h) $ elems $ choose (length S.cardLst) 5

--intsToCards :: [Int] -> [S.Card]
--intsToCards l = map toEnum l

choose :: Ord r => Int -> [r] -> [[r]]
choose 0 lst = [[]]
choose n lst = do
  (x:xs) <- tails lst
  rest <- choose (n-1) xs
  return $ x : rest

chosen = choose 2 S.cardLst

