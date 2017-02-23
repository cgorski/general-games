import Test.HUnit
import Game.Game.Poker
import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker

-- allHands = allPossibleHands
allHandsCount = length allPossibleHands
allHandsCountExpected = 2598960

royalFlush =
  [PlayingCard Ace Hearts,
   PlayingCard Queen Hearts,
   PlayingCard King Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]

royalFlushNot =
  [PlayingCard Ace Hearts,
   PlayingCard Queen Hearts,
   PlayingCard Eight Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]


-- royalFlushLstAH = AH.fromStandardCardLst royalFlushLst
-- royalFlushLstAL = AL.fromStandardCardLst royalFlushLst

sortedRoyalFlush =
  [PlayingCard Ace Hearts,
   PlayingCard King Hearts,
   PlayingCard Queen Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]


  
testPossibleHands = TestCase (assertEqual "Total number of poker hands" allHandsCountExpected allHandsCount)
testPossibleRoyalFlush = TestCase (assertEqual "Total number of royal flushes" 4 (length allRoyalFlush))
testPossibleStraightFlush = TestCase (assertEqual "Total number of straight flushes" 36 (length allStraightFlush))
testPossibleFourOfAKind = TestCase (assertEqual "Total number of four-of-a-kinds" 624 (length allFourOfAKind))
testMkRoyalFlush = TestCase (assertEqual "Is [AH, KH, QH, JH, TH] a Royal Flush" (Just $ PokerHand RoyalFlush sortedRoyalFlush) (mkRoyalFlush royalFlush))
testIsRoyalFlush = TestCase (assertEqual "Is [AH, QH, KH, JH, TH] a Royal Flush" True (isRoyalFlush royalFlush))
testIsRoyalFlushNot = TestCase (assertEqual "Is [AH, QH, 8H, JH, TH] a Royal Flush" False (isRoyalFlush royalFlushNot))
-- testIsMinHandSize = TestCase (assertEqual "Is min size of 5" True (isMinHandSize royalFlush))
-- testIsMinHandSize2 = TestCase (assertEqual "Is min size of 5" False (isMinHandSize $ DS.fromList [S.Card S.Ace S.Spades]))
-- testSortHighToLow = TestCase (assertEqual "Is sorted from high to low"
--                                sortedRoyalFlushLst
--                                (sortHighToLow $ AH.fromStandardCardLst royalFlushLst))
-- testIsSameSuit = TestCase (assertEqual "Is same suit" True (isSameSuit $ DS.fromList royalFlushLst))    
-- --testHasConsecutiveRanks = TestCase (assertEqual "Is consecutive" True (AH.fromStdroyalFlush

                 
tests = TestList [
  TestLabel "Test for testPossibleHands" testPossibleHands,
  TestLabel "Test for testPossibleRoyalFlush" testPossibleRoyalFlush,
  TestLabel "Test for testPossibleStraightFlush" testPossibleStraightFlush,
  TestLabel "Test for testPossibleFourOfAKind" testPossibleFourOfAKind,
  TestLabel "Test for mkRoyalFlush" testMkRoyalFlush,
  TestLabel "Test for isRoyalFlush" testIsRoyalFlush,
  TestLabel "Test for isRoyalFlushNot" testIsRoyalFlushNot
  ]
--   TestLabel "Test for isMinHandSize royalFlush" testIsMinHandSize,
--   TestLabel "Test for isMinHandSize singleton" testIsMinHandSize2,
--   TestLabel "Test for sortHighToLow royalFlush" testSortHighToLow,
--   TestLabel "Test for isSameSuit royalFlush" testIsSameSuit]
main :: IO ()
main =
  do
--    putStrLn $ show $ map (fromEnum . S.toRank) $ sort royalFlushLstAH
--    putStrLn $ show $ map (fromEnum . S.toRank) $ sort royalFlushLstAL
--    putStrLn $ show $ allFourOfAKind
    counts <- runTestTT tests
    putStrLn $ show counts
--    putStrLn $ show $ sortCardsBy AceHighRankOrder royalFlush

    
