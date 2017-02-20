module Main where

import Game.Implement.Card.Standard 

main :: IO ()
main = do
  let r = Ace
      s = Clubs
      c = mkCard r s
      min = minBound :: Rank
      max = maxBound :: Rank
      l = cardLst
      n = numCards
  putStrLn (show c)
  putStrLn (show min)
  putStrLn (show max)
  putStrLn $ show l
  putStrLn $ show n
  
