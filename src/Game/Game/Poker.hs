module Game.Game.Poker
  where

import qualified Game.Implement.Card.Standard as S
import qualified Game.Implement.Card.Standard.Poker.AceHigh as AH
import qualified Game.Implement.Card.Standard.Poker.AceLow as AL
import qualified Data.Set as DS

type Hand a = DS.Set a

data PokerHandOrderedKind =
  HighCard (Hand AH.Card)
  | Pair (Hand AH.Card)
  | TwoPair (Hand AH.Card)
  | ThreeOfAKind (Hand AH.Card)
  | StraightAH (Hand AH.Card)
  | StraightAL (Hand AL.Card)
  | Flush (Hand AH.Card)
  | FullHouse (Hand AH.Card)
  | FourOfAKind (Hand AH.Card)
  | StraightFlushAH (Hand AH.Card)
  | StraightFlushAL (Hand AL.Card)
  | RoyalFlush (Hand AH.Card)

mkBestHand :: (Ord c, S.OrderedCard c) => [c] -> Hand c
mkBestHand clst = DS.fromList clst


