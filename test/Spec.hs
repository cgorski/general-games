import Test.HUnit
import Game.Game.Poker
import Game.Implement.Card
import Game.Implement.Card.Standard


allHandsCount :: Int
allHandsCount = length allPossibleHands
allHandsCountExpected :: Int
allHandsCountExpected = 2598960
allRoyalFlushCountExpected :: Int
allRoyalFlushCountExpected = 4
allStraightFlushCountExpected :: Int
allStraightFlushCountExpected = 36
allFourOfAKindCountExpected :: Int
allFourOfAKindCountExpected = 624
allFullHouseCountExpected :: Int
allFullHouseCountExpected = 3744
allFlushCountExpected :: Int
allFlushCountExpected = 5108
allStraightCountExpected :: Int
allStraightCountExpected = 10200
allThreeOfAKindCountExpected :: Int
allThreeOfAKindCountExpected = 54912
allTwoPairCountExpected :: Int
allTwoPairCountExpected = 123552
allPairCountExpected :: Int
allPairCountExpected = 1098240
allHighCardCountExpected :: Int
allHighCardCountExpected = 1302540

royalFlush :: [PlayingCard]
royalFlush =
  [PlayingCard Ace Hearts,
   PlayingCard Queen Hearts,
   PlayingCard King Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]

royalFlushNot :: [PlayingCard]
royalFlushNot =
  [PlayingCard Ace Hearts,
   PlayingCard Queen Hearts,
   PlayingCard Eight Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]

drawDeck :: [PlayingCard]
drawDeck =
  [PlayingCard Five Diamonds,
   PlayingCard Seven Clubs,
   PlayingCard Two Spades,
   PlayingCard King Spades,
   PlayingCard King Hearts,
   PlayingCard Ace Diamonds,
   PlayingCard Seven Diamonds,
   PlayingCard Three Clubs,
   PlayingCard Four Clubs]


drawDeckSizes :: [Int]
drawDeckSizes = [1,4,2]

drawDeckExpectedOutput :: Maybe ([[PlayingCard]],[PlayingCard])
drawDeckExpectedOutput = Just
  ([[PlayingCard Five Diamonds],
   [PlayingCard Seven Clubs,
   PlayingCard Two Spades,
   PlayingCard King Spades,
   PlayingCard King Hearts],
   [PlayingCard Ace Diamonds,
   PlayingCard Seven Diamonds]],
   [PlayingCard Three Clubs,
   PlayingCard Four Clubs])

  

testCardDraw :: Test
testCardDraw = TestCase (assertEqual "testCardDraw test" drawDeckExpectedOutput (draw drawDeckSizes drawDeck))

testIsRoyalFlush :: Test
testIsRoyalFlush = TestCase (assertEqual "Is [AH, QH, KH, JH, TH] a Royal Flush" True (isRoyalFlush royalFlush))
testIsRoyalFlushNot :: Test
testIsRoyalFlushNot = TestCase (assertEqual "Is [AH, QH, 8H, JH, TH] a Royal Flush" False (isRoyalFlush royalFlushNot))



testPossibleHands :: Test
testPossibleHands = TestCase (assertEqual "Total number of poker hands" allHandsCountExpected allHandsCount)
testPossibleRoyalFlush :: Test
testPossibleRoyalFlush = TestCase (assertEqual "Total number of royal flushes" allRoyalFlushCountExpected (length allRoyalFlush))
testPossibleStraightFlush :: Test
testPossibleStraightFlush = TestCase (assertEqual "Total number of straight flushes" allStraightFlushCountExpected (length allStraightFlush))
testPossibleFourOfAKind :: Test
testPossibleFourOfAKind = TestCase (assertEqual "Total number of four-of-a-kinds" allFourOfAKindCountExpected (length allFourOfAKind))
testPossibleFullHouse :: Test
testPossibleFullHouse = TestCase (assertEqual "Total number of full houses" allFullHouseCountExpected (length allFullHouse))
testPossibleFlush :: Test
testPossibleFlush = TestCase (assertEqual "Total number of flushes" allFlushCountExpected (length allFlush))
testPossibleStraight :: Test
testPossibleStraight = TestCase (assertEqual "Total number of straights" allStraightCountExpected (length allStraight))
testPossibleThreeOfAKind :: Test
testPossibleThreeOfAKind = TestCase (assertEqual "Total number of three-of-a-kinds" allThreeOfAKindCountExpected (length allThreeOfAKind))
testPossibleTwoPair :: Test
testPossibleTwoPair = TestCase (assertEqual "Total number of two-pairs" allTwoPairCountExpected (length allTwoPair))
testPossiblePair :: Test
testPossiblePair = TestCase (assertEqual "Total number of pairs" allPairCountExpected (length allPair))
testPossibleHighCard :: Test
testPossibleHighCard = TestCase (assertEqual "Total number of high cards" allHighCardCountExpected (length allHighCard))


tests :: Test
tests = TestList [
  -- Card class functions
  TestLabel "Test for Card.draw" testCardDraw,
  
  -- Test boolean hand checks
  TestLabel "Test for testIsRoyalFlush" testIsRoyalFlush,
  TestLabel "Test for testIsRoyalFlushNot" testIsRoyalFlushNot,
  
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
    c <- runTestTT tests
    putStrLn $ show c



    
