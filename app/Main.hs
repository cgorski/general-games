module Main where

import Game.Implement.Card.Standard
import Game.Game.Poker

main :: IO ()
main =
  let r = Ace
      s = Clubs
      cards = mkCard r s
      min = minBound :: Rank
      max = maxBound :: Rank
      l = cardLst
      n = numCards in
    do
      putStrLn $ show cards
--      putStrLn $ show min
--      putStrLn $ show max
--      putStrLn $ show l
--      putStrLn $ show n
--      putStrLn $ show $ take 100 allPossibleHands
--      putStrLn $ show (length allPossibleHands)
--      putStrLn $ show (length allPossibleHands)
--      putStrLn $ show (length allPossibleHands)
--      putStrLn $ show (length allPossibleHands)
      putStrLn $ show allRoyalFlush
      putStrLn $ show (length allStraightFlush)
      putStrLn $ show (length allFourOfAKind)
      
  
