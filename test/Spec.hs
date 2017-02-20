import Test.HUnit
import Game.Game.Poker

allHands = allPossibleHands
allHandsCount = length allPossibleHands
allHandsCountExpected = 2598960
  
testPossibleHands = TestCase (assertEqual "1 == 1" allHandsCount allHandsCountExpected)

tests = TestList [
  TestLabel "Test for testPossibleHands" testPossibleHands]

main :: IO ()
main =
  do
    counts <- runTestTT tests
    putStrLn $ show counts
    
