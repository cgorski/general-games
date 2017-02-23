import Test.HUnit
import Game.Game.Poker
import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker

-- allHands = allPossibleHands
allHandsCount = length allPossibleHands
allHandsCountExpected = 2598960
allRoyalFlushCountExpected = 4
allStraightFlushCountExpected = 36
allFourOfAKindCountExpected = 624
allFullHouseCountExpected = 3744
allFlushCountExpected = 5108
allStraightCountExpected = 10200
allThreeOfAKindCountExpected = 54912
allTwoPairCountExpected = 123552
allPairCountExpected = 1098240
allHighCardCountExpected = 1302540

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

singlePair =
  [PlayingCard Three Clubs,
   PlayingCard Three Spades,
   PlayingCard Eight Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]

twoPair =
  [PlayingCard Three Clubs,
   PlayingCard Three Spades,
   PlayingCard Four Hearts,
   PlayingCard Four Diamonds,
   PlayingCard Ten Hearts]

sortedRoyalFlush =
  [PlayingCard Ace Hearts,
   PlayingCard King Hearts,
   PlayingCard Queen Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]

testIsRoyalFlush = TestCase (assertEqual "Is [AH, QH, KH, JH, TH] a Royal Flush" True (isRoyalFlush royalFlush))
testIsRoyalFlushNot = TestCase (assertEqual "Is [AH, QH, 8H, JH, TH] a Royal Flush" False (isRoyalFlush royalFlushNot))

testPossibleHands = TestCase (assertEqual "Total number of poker hands" allHandsCountExpected allHandsCount)
testPossibleRoyalFlush = TestCase (assertEqual "Total number of royal flushes" allRoyalFlushCountExpected (length allRoyalFlush))
testPossibleStraightFlush = TestCase (assertEqual "Total number of straight flushes" allStraightFlushCountExpected (length allStraightFlush))
testPossibleFourOfAKind = TestCase (assertEqual "Total number of four-of-a-kinds" allFourOfAKindCountExpected (length allFourOfAKind))
testPossibleFullHouse = TestCase (assertEqual "Total number of full houses" allFullHouseCountExpected (length allFullHouse))
testPossibleFlush = TestCase (assertEqual "Total number of flushes" allFlushCountExpected (length allFlush))
testPossibleStraight = TestCase (assertEqual "Total number of straights" allStraightCountExpected (length allStraight))
testPossibleThreeOfAKind = TestCase (assertEqual "Total number of three-of-a-kinds" allThreeOfAKindCountExpected (length allThreeOfAKind))
testPossibleTwoPair = TestCase (assertEqual "Total number of two-pairs" allTwoPairCountExpected (length allTwoPair))
testPossiblePair = TestCase (assertEqual "Total number of pairs" allPairCountExpected (length allPair))
testPossibleHighCard = TestCase (assertEqual "Total number of high cards" allHighCardCountExpected (length allHighCard))


                 
tests = TestList [
  -- Test boolean hand checks
  TestLabel "Test for testIsRoyalFlush" testIsRoyalFlush,
  
  -- Test that hand calculation counts match correct values
  TestLabel "Test for testPossibleHands" testPossibleHands,
  TestLabel "Test for testPossibleRoyalFlush" testPossibleRoyalFlush,
  TestLabel "Test for testPossibleStraightFlush" testPossibleStraightFlush,
  TestLabel "Test for testPossibleFourOfAKind" testPossibleFourOfAKind,
  TestLabel "Test for testPossibleFullHouse" testPossibleFullHouse,
  TestLabel "Test for testPossibleFlush" testPossibleFlush,
  TestLabel "Test for testPossibleStraight" testPossibleStraight,
  TestLabel "Test for testPossibleThreeOfAKind" testPossibleThreeOfAKind,
  TestLabel "Test for testTwoPair" testPossibleTwoPair,
  TestLabel "Test for testPair" testPossiblePair,
  TestLabel "Test for testHighCard" testPossibleHighCard
  ]

main :: IO ()
main =
  do
    counts <- runTestTT tests
    putStrLn $ show counts


    
