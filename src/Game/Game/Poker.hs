module Game.Game.Poker
  where

import qualified Game.Implement.Card.Standard as S
import qualified Game.Implement.Card.Standard.Poker.AceHigh as AH
import qualified Game.Implement.Card.Standard.Poker.AceLow as AL
import qualified Data.Set as DS
import Data.Choose

type Hand = DS.Set S.Card

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

c = [x | x <- mapM (const "ABCD") [1..2], head x < head (tail x)]
