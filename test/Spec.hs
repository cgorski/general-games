import Test.Hspec
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

drawDeckSizesFail :: [Int]
drawDeckSizesFail = [5,9]

drawDeckSizesFailNeg :: [Int]
drawDeckSizesFailNeg = [-3,4]


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

main :: IO ()
main = hspec $ do
  describe "Game.Implement.Card instance" $ do
    it "returns drawn hands from a deck, plus the remaining deck" $ do
      (draw drawDeckSizes drawDeck) `shouldBe` drawDeckExpectedOutput
    it "returns Nothing when trying to return more cards than in deck" $ do
      (draw drawDeckSizesFail drawDeck) `shouldBe` Nothing
    it "returns Nothing when trying to return negative cards" $ do
      (draw drawDeckSizesFailNeg drawDeck) `shouldBe` Nothing

  describe "Game.Implement.Card.Standard.Poker.isRoyalFlush" $ do
    it "confirms that [AH, QH, KH, JH, TH] is a Royal Flush" $ do
      (isRoyalFlush royalFlush) `shouldBe` True
    it "confirms that [AH, QH, 8H, JH, TH] is not a Royal Flush" $ do
      (isRoyalFlush royalFlushNot) `shouldBe` False
  describe "Game.Implement.Card.Standard.Poker allPossibleHands / isHand functions" $ do
    it "confirms the total number of poker hands" $ do
      allHandsCount `shouldBe` allHandsCountExpected
    it "confirms the total number of royal flushes" $ do
      (length allRoyalFlush) `shouldBe` allRoyalFlushCountExpected
    it "confirms the total number of straight flushes" $ do
      (length allStraightFlush) `shouldBe` allStraightFlushCountExpected
    it "confirms the total number of four-of-a-kinds" $ do
      (length allFourOfAKind) `shouldBe` allFourOfAKindCountExpected
    it "confirms the total number of full houses" $ do
      (length allFullHouse) `shouldBe` allFullHouseCountExpected
    it "confirms the total number of flushes" $ do
      (length allFlush) `shouldBe` allFlushCountExpected
    it "confirms the total number of straights" $ do
      (length allStraight) `shouldBe` allStraightCountExpected
    it "confirms the total number of three-of-a-kinds" $ do
      (length allThreeOfAKind) `shouldBe` allThreeOfAKindCountExpected
    it "confirms the total number of two-pairs" $ do
      (length allTwoPair) `shouldBe` allTwoPairCountExpected
    it "confirms the total number of pairs" $ do
      (length allPair) `shouldBe` allPairCountExpected
    it "confirms the total number of high card hands" $ do
      (length allHighCard) `shouldBe` allHighCardCountExpected

