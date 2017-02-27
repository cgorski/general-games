import Control.Monad.Random
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

confirmDisjoint :: (Int, Bool)
confirmDisjoint =
  let mfunc1 hand = [mkRoyalFlush hand,
                     mkStraightFlush hand,
                     mkFourOfAKind hand,
                     mkFullHouse hand,
                     mkFlush hand,
                     mkStraight hand,
                     mkThreeOfAKind hand,
                     mkTwoPair hand,
                     mkPair hand,
                     mkHighCard hand]
      maybem (Just _) = 1
      maybem Nothing = 0
      countJust hand = sum $ map maybem $ mfunc1 hand
      allSums = map countJust allPossibleHands
      collect _  (outsum, False) = (outsum, False)
      collect (x:xs) (outsum, _) =
        collect xs (x+outsum, if x==0 || x==1 then True else False)
      collect [] output = output 
  in
    collect allSums (0, True)

isUnique :: Eq a => [a] -> Bool
isUnique lst = f lst True where
  f _ False = False
  f [] result = result
  f (x:xs) _ = if x `elem` xs then f xs False else f xs True

shuffledDeck :: RandomGen g => Rand g [PlayingCard]
shuffledDeck = shuffle $ fullDeck

main :: IO ()
main =
  do
    randdecks <- evalRandIO $ replicateM 10000 shuffledDeck;

    randStraights <-
      let
        r = do
          h <- randomStraight
          return (h, isStraight $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randFourOfAKinds <-
      let
        r = do
          h <- randomFourOfAKind
          return (h, isFourOfAKind $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r


    randStraightFlushes <-
      let
        r = do
          h <- randomStraightFlush
          return (h, isStraightFlush $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r



    

    hspec $ do
      describe "Game.Implement.Card.draw (PlayingCard)" $ do
        it "returns drawn hands from a deck, plus the remaining deck" $ do
          (draw drawDeckSizes drawDeck) `shouldBe` drawDeckExpectedOutput
        it "returns Nothing when trying to return more cards than in deck" $ do
          (draw drawDeckSizesFail drawDeck) `shouldBe` Nothing
        it "returns Nothing when trying to return negative cards" $ do
          (draw drawDeckSizesFailNeg drawDeck) `shouldBe` Nothing

      describe "Game.Implement.Card.fullDeck (PlayingCard)" $ do
        it "returns 52 cards" $ do
          length (fullDeck :: [PlayingCard]) `shouldBe` 52
        it "returns unique cards" $ do
          isUnique (fullDeck :: [PlayingCard]) `shouldBe` True

  
      describe "Game.Implement.Card.Standard.Poker.isRoyalFlush" $ do
        it "confirms that [AH, QH, KH, JH, TH] is a Royal Flush" $ do
          (isRoyalFlush royalFlush) `shouldBe` True
        it "confirms that [AH, QH, 8H, JH, TH] is not a Royal Flush" $ do
          (isRoyalFlush royalFlushNot) `shouldBe` False

      describe "Game.Game.Poker.randomStraight" $ do
        it "returns 100000 random Straights" $ do 
          randStraights `shouldBe` (map (\(h,_) -> (h,True)) randStraights) 

        it "returns 100000 random Straight Flushes" $ do 
          randStraightFlushes `shouldBe` (map (\(h,_) -> (h,True)) randStraightFlushes) 

        it "returns 100000 random Four-of-a-Kinds" $ do 
          randFourOfAKinds `shouldBe` (map (\(h,_) -> (h,True)) randFourOfAKinds)


      -- describe "Game.Implement.Card.shuffle (PlayingCard)" $ do
      --   it "returns 10000 different fullDeck shuffles using the global random generator" $ do
      --     (isUnique randdecks) `shouldBe` True

      -- describe "Game.Implement.Card.Standard.Poker allPossibleHands / mkHand / isHand functions" $ do
      --   it "confirms that sets of each hand are disjoint and that total count correct" $ do
      --     confirmDisjoint `shouldBe` (allHandsCountExpected, True)
      --   it "confirms the total number of poker hands" $ do
      --     allHandsCount `shouldBe` allHandsCountExpected
      --   it "confirms the total number of royal flushes" $ do
      --     (length allRoyalFlush) `shouldBe` allRoyalFlushCountExpected
      --   it "confirms the total number of straight flushes" $ do
      --     (length allStraightFlush) `shouldBe` allStraightFlushCountExpected
      --   it "confirms the total number of four-of-a-kinds" $ do
      --     (length allFourOfAKind) `shouldBe` allFourOfAKindCountExpected
      --   it "confirms the total number of full houses" $ do
      --     (length allFullHouse) `shouldBe` allFullHouseCountExpected
      --   it "confirms the total number of flushes" $ do
      --     (length allFlush) `shouldBe` allFlushCountExpected
      --   it "confirms the total number of straights" $ do
      --     (length allStraight) `shouldBe` allStraightCountExpected
      --   it "confirms the total number of three-of-a-kinds" $ do
      --     (length allThreeOfAKind) `shouldBe` allThreeOfAKindCountExpected
      --   it "confirms the total number of two-pairs" $ do
      --     (length allTwoPair) `shouldBe` allTwoPairCountExpected
      --   it "confirms the total number of pairs" $ do
      --     (length allPair) `shouldBe` allPairCountExpected
      --   it "confirms the total number of high card hands" $ do
      --     (length allHighCard) `shouldBe` allHighCardCountExpected

