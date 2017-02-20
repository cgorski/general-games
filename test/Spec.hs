import Test.HUnit
import Game.Game.Poker
import qualified Game.Implement.Card.Standard as S
import Data.Set as DS

allHands = allPossibleHands
allHandsCount = length allPossibleHands
allHandsCountExpected = 2598960

royalFlushLst =
  [S.Card S.Ace S.Hearts,
   S.Card S.Queen S.Hearts,
   S.Card S.King S.Hearts,
   S.Card S.Jack S.Hearts,
   S.Card S.Ten S.Hearts]

sortedRoyalFlushLst =
  [S.Card S.Ace S.Hearts,
   S.Card S.King S.Hearts,
   S.Card S.Queen S.Hearts,
   S.Card S.Jack S.Hearts,
   S.Card S.Ten S.Hearts]

royalFlush = DS.fromList royalFlushLst
  
testPossibleHands = TestCase (assertEqual "Total number of poker hands" allHandsCountExpected allHandsCount)
testMkRoyalFlush = TestCase (assertEqual "Is [AS, KS, QS, JS, TS] a Royal Flush" (Just $ RoyalFlush royalFlush) (mkRoyalFlush royalFlush))
testIsMinHandSize = TestCase (assertEqual "Is min size of 5" True (isMinHandSize royalFlush))
testIsMinHandSize2 = TestCase (assertEqual "Is min size of 5" False (isMinHandSize $ DS.fromList [S.Card S.Ace S.Spades]))
testSortHighToLow = TestCase (assertEqual "Is sorted from high to low"
                               sortedRoyalFlushLst
                               (sortHighToLow royalFlushLst))
testIsSameSuit = TestCase (assertEqual "Is same suit" True (isSameSuit royalFlushLst))    
                               
tests = TestList [
  TestLabel "Test for testPossibleHands" testPossibleHands,
  TestLabel "Test for mkRoyalFlush" testMkRoyalFlush,
  TestLabel "Test for isMinHandSize royalFlush" testIsMinHandSize,
  TestLabel "Test for isMinHandSize singleton" testIsMinHandSize2,
  TestLabel "Test for sortHighToLow royalFlush" testSortHighToLow,
  TestLabel "Test for isSameSuit royalFlush" testIsSameSuit]

main :: IO ()
main =
  do
    counts <- runTestTT tests
    putStrLn $ show counts
    
