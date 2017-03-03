{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}


-- |
-- Module      : Game.Implement.Evolution
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Implement.Evolution module provides fundamental operations for evolutionary algorthims.
module Game.Implement.Evolution
  (

  )
  where

import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import Data.List (nub, maximumBy, minimumBy, sortBy, foldl1', tails)

data Operation =
  Add
  | Subtract
  | Multiply
  | Divide
  | Nop1
  | Nop0
  deriving (Bounded, Enum, Show, Eq)
  
data Tree o v = Leaf v | Node o (Tree o v) (Tree o v) deriving (Show)

data LeafType v = Value v

data Direction = L | R deriving (Enum, Show)

evalTree :: (Num v, Fractional v) => Tree Operation v -> v
evalTree (Leaf v) = v
evalTree (Node Add l r) = (evalTree l) + (evalTree r)
evalTree (Node Subtract l r) = (evalTree l) - (evalTree r)
evalTree (Node Multiply l r) = (evalTree l) * (evalTree r)
evalTree (Node Divide l r) = (evalTree l) / (evalTree r)
evalTree (Node Nop1 l r) = 1
evalTree (Node Nop0 l r) = 0

randomLst :: RandomGen g => Rand g [(Operation, LeafType Int, Direction)]
randomLst =
  do
    randOp <- getRandomR(0,5)
    randValue <- getRandomR(0,10)
    randDirection <- getRandomR(0,1)
    value <- return (toEnum randOp, Value randValue, toEnum randDirection)
    rlst <- randomLst
    return $ value:rlst

data Crumb o v = LeftCrumb o (Tree o v) | RightCrumb o (Tree o v)
type Breadcrumbs o v = [Crumb o v]
type Zipper o v = (Tree o v, Breadcrumbs o v)

randomTree :: RandomGen g => Int -> Double -> Double -> Rand g (Tree Operation Double)
randomTree nodeSize minNum maxNum = 
  let
    
    nTree :: Tree Operation Double -> Direction -> Operation -> Double -> Tree Operation Double
    nTree node L op value = (Node op (Leaf value) node) 
    nTree node R op value = (Node op node (Leaf value))

    randNum :: RandomGen g => Rand g Double
    randNum = getRandomR(minNum,maxNum)

    randDir :: RandomGen g => Rand g Direction
    randDir = do
      rNum :: Int <- getRandomR(0,1)
      return $ if rNum == 0 then L else R

    randOp :: RandomGen g => Rand g Operation
    randOp = do 
      rNum :: Int <- getRandomR(0,5)
      return $ toEnum rNum

    f :: RandomGen g => Tree Operation Double -> Int -> Rand g (Tree Operation Double)

    f node 0 = do
      return node
      
    f node ns = do
      rNum <- randNum
      rDir <- randDir
      rOp <- randOp
      
      newTree <- return $ nTree node rDir rOp rNum
      f newTree (ns-1)
      
  in do
    n <- randNum 
    startLeaf <- return $ Leaf n
    f startLeaf nodeSize
    

    
    

-- mkRdTree :: RandomGen g => Rand g [(Operation, LeafType Int, Direction)] -> Rand Tree Operation LeafType
-- mkRdTree lst =
--   let
--     f (Node o) bcrumb = 

-- rLst :: RandomGen g => Rand g [Int]
-- rLst =
--   do
--     val <- getRandomR(0,10)
--     rlst <- rLst
--     return $ val:rlst

-- rLst :: RandomGen g => Rand g [(Int,Int)]
-- rLst =
--   do
--     val <- getRandomR(0,10)
--     val1 <- getRandomR(0,10)
    
get5 :: RandomGen g => Rand g [Int] -> Rand g [Int]
get5 l = do
  st <- l
  return $ map (+9) $ take 100 st
  
-- randomTree :: RandomGen g => Rand g [Int] -> Rand g [Tree Operation LeafType]
-- randomTree rlst =
--   let
--     f (r:ns) 





