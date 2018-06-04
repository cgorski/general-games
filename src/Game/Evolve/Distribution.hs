{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Probability.Distribution
-- Copyright   :  (c) William Yager 2015
-- License     :  MIT
-- Maintainer  :  will (dot) yager (at) gmail (dot) com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a data structure and associated functions for
-- representing discrete probability distributions.
--
-- All time and space complexity metrics are given in terms of @n@. In this 
-- case, @n@ refers to the number of unique outcomes inserted into the tree.
-- If one were to construct a tree by inserting a billion of the same
-- outcome, @n@ would still be 1.
-- 
-- The data structure is optimized for fast sampling from the distribution.
-- Sampling ranges from @O(1)@ to @O(log(n))@ depending on the distribution.
--
-- Under the hood, the distribution is represented by a perfectly balanced
-- binary tree. The tree enforces a heap property, where more likely outcomes
-- are closer to the top than less likely outcomes. Because we're more 
-- likely to sample from those outcomes, we minimize the amount of time
-- spent traversing the tree.
--
-- When a duplicate outcome is inserted into the tree, the tree's "dups"
-- counter is incremented. When more than half the tree is duplicate entries,
-- the entire tree is rebuilt from scratch. Using amortized complexity 
-- analysis, we can show that insertion is, at worst, @log(n)@ amortized
-- complexity. This prevents the size of tree from increasing to more than
-- @O(n)@, even with many duplicate outcomes inserted.
-----------------------------------------------------------------------------

module Game.Evolve.Distribution (
    -- * The distribution type
    Distribution,
    -- * Probability operations
    sample,
    cumulate,
    normalize,
    lookup,
    -- * Building
    empty,
    insert,
    fromList,
    -- * Modifying
    reweight,
    -- * Reducing
    toList,
    foldrWithP,
    -- * Combining
    joint,
    sum,
    -- * Debugging
    invariants
) where

import Prelude hiding (product, sum, lookup)
import Control.Monad.Random (MonadRandom, Random, getRandomRs)
import Data.Word (Word)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

-- | A probability distribution with probabilities of type @p@ and
-- outcomes/events of type @o@.
data Distribution p o = Distribution !(DTree p o) !(Map.Map o p) !Word


data DTree p o = Leaf
               | DTree !o !p !p !Word !(DTree p o) !(DTree p o)

outcomeOf (DTree o _ _ _ _ _) = o
probOf    Leaf                = 0
probOf    (DTree _ p _ _ _ _) = p
sumOf     Leaf                = 0
sumOf     (DTree _ _ s _ _ _) = s
countOf   Leaf                = 0
countOf   (DTree _ _ _ c _ _) = c
leftOf    (DTree _ _ _ _ l _) = l
rightOf   (DTree _ _ _ _ _ r) = r

instance (Num p, Show p, Ord o, Show o) => Show (Distribution p o) where
    show dist = "fromList " ++ show (toList dist)

-- | Reweights the probabilities in the distribution based on
-- the given function. @n*log(n)@
reweight :: (Ord o, Num p, Ord p) => ((o,p) -> p) -> Distribution p o -> Distribution p o
reweight f = fromUniqList . map update . toList
    where update (o,p) = (o, f (o,p))

-- | The sum of all probabilities in the distribution. @O(1)@
cumulate :: (Num p) => Distribution p o -> p
cumulate (Distribution tree _ _) = sumOf tree

-- | Normalizes the distribution.
-- After normalizing, @'cumulate' distribution@ is 1. @O(n)@
normalize :: (Fractional p) => Distribution p o -> Distribution p o
normalize (Distribution Leaf _ _) = error "Can't normalize empty distribution"
normalize (Distribution tree@(DTree _ _ sum _ _ _) members dups) =
    Distribution (normalize' sum tree) (Map.map (/ sum) members) dups
normalize' sum Leaf                = Leaf
normalize' sum (DTree e p s c l r) = DTree e (p/sum) (s/sum) c l' r'
    where
    l' = normalize' sum l
    r' = normalize' sum r

-- | Insert an outcome into the distribution.
-- Inserting @(o,p1)@ and @(o,p2)@ results in the same sampled distribution as
-- inserting @(o,p1+p2)@. @O(log(n))@ amortized.
insert :: (Ord o, Num p, Ord p) => (o,p) -> Distribution p o -> Distribution p o
insert (_, 0)  dist                              = dist
insert (o',p') (Distribution tree outcomes dups) = if dups' * 2 <= countOf tree
    then distribution' -- Not too many repeated elements
    else fromUniqList . toList $ distribution'
    where
    dups' = if Map.member o' outcomes then dups + 1 else dups
    outcomes' = Map.insertWith (+) o' p' outcomes
    tree' = insertTree (o',p') tree
    distribution' = Distribution tree' outcomes' dups'

-- | The empty distribution. @O(1)@
empty :: (Num p) => Distribution p o
empty = Distribution Leaf Map.empty 0

reduce :: (Ord o, Num p) => [(o,p)] -> Map.Map o p
reduce = foldl' (\map (o,p) -> Map.insertWith (+) o p map) Map.empty
-- | @O(n*log(n))@ 
fromList :: (Ord o, Num p, Ord p) => [(o,p)] -> Distribution p o
fromList = fromUniqList . Map.toList . reduce
-- | @O(n*log(n))@
toList :: (Ord o, Num p) => Distribution p o -> [(o,p)]
toList = Map.toList . reduce . toRepeatList

-- | Doesn't bother to remove duplicates. @O(n*log(n))@ amortized.
fromUniqList :: (Ord o, Num p, Ord p) => [(o,p)] -> Distribution p o
fromUniqList = foldl' (\dist pair -> insert pair dist) empty
-- | Doesn't bother to eliminate repeats. @O(n)@
toRepeatList :: Distribution p o -> [(o,p)]
toRepeatList = foldrWithP (:) []

-- | A right-associative fold on the tree structure, including the
-- probabilities. Note that outcomes may be repeated within the data structure.
-- If you want identical outcomes to be lumped together, fold on the list 
-- produced by @'toList'@. @O(n)@.
foldrWithP :: ((o,p) -> b -> b) -> b -> Distribution p o -> b
foldrWithP f b (Distribution tree _ _) = foldrTreeWithP f b tree

foldrTreeWithP :: ((o,p) -> b -> b) -> b -> DTree p o -> b
foldrTreeWithP f b Leaf = b
foldrTreeWithP f b (DTree o p _ _ l r) = foldrTreeWithP f (f (o,p) (foldrTreeWithP f b r)) l

insertTree :: (Num p, Ord p) => (o,p) -> DTree p o -> DTree p o
insertTree (o',p') Leaf = DTree o' p' p' 1 Leaf Leaf
insertTree (o',p') (DTree o p s c l r)
    | p' <= p = if countOf l < countOf r
        then DTree o  p  s' c' (insertTree (o',p') l) r
        else DTree o  p  s' c' l                      (insertTree (o',p') r)
    | p' >  p = if countOf l < countOf r
        then DTree o' p' s' c' (insertTree (o,p)   l) r
        else DTree o' p' s' c' l                      (insertTree (o,p)   r)
    where
    s' = s + p'
    c' = c + 1

-- | Creates a new distribution that's the joint distribution of the two provided.
-- @O(nm*log(nm))@ amortized.
joint :: (Ord o1, Ord o2, Num p, Ord p) => Distribution p o1 -> Distribution p o2 -> Distribution p (o1, o2)
joint da db = fromList $ [((a,b), pa * pb) | 
                          (a,pa) <- toList da, 
                          (b,pb) <- toList db]

-- | Creates a new distribution by summing the probabilities of the outcomes
-- in the two provided. @O((n+m)log(n+m))@ amortized.
sum :: (Ord o, Num p, Ord p) => Distribution p o -> Distribution p o -> Distribution p o
sum da db = fromList $ toRepeatList da ++ toRepeatList db

-- Returns random value in range (0,n]
randomPositiveUpto :: (Eq n, Num n, Random n, MonadRandom m) => n -> m n
randomPositiveUpto n = do
    randoms <- getRandomRs (0,n)
    return . head . dropWhile (==0) $ randoms

-- | Given an outcome, returns the probability. Note that the probability
-- is not always normalized. If you want the probability to be in the 0-1
-- range, you should divide it by @cumulate dist@ (the sum of the probability
-- of all outcomes)
lookup :: (Ord o, Num p) => Distribution p o -> o -> p
lookup (Distribution _ members _) outcome = case Map.lookup outcome members of
    Nothing -> 0
    Just probability -> probability

-- | Take a sample from the distribution. Can be used with e.g. @evalRand@
-- or @evalRandIO@ from @Control.Monad.Random@. @O(log(n))@ for a uniform 
-- distribution (worst case), but approaches @O(1)@ with less balanced
-- distributions.
sample :: (Ord p, Num p, Random p, MonadRandom m) => Distribution p o -> m o
sample (Distribution tree _ _) = sampleTree tree

sampleTree :: (Ord p, Num p, Random p, MonadRandom m) => DTree p o -> m o
sampleTree Leaf = error "Error: Can't sample an empty distribution"
sampleTree (DTree event prob sum count l r) = do
    index <- randomPositiveUpto sum
    let result | index > sumOf l + prob = sampleTree r
               | index > sumOf l        = return event
               | index > 0              = sampleTree l
    result

sizeInvariant :: (Num p, Eq p) => DTree p o -> Either String ()
sizeInvariant Leaf = Right ()
sizeInvariant (DTree e p s c l r)
    | (c /= countOf l + countOf r + 1) = Left $ "Count mismatch"
    | (countOf l > countOf r + 1) = Left $ "Left is too heavy"
    | (countOf r > countOf l + 1) = Left $ "Right is too heavy"
    | otherwise = sizeInvariant l >> sizeInvariant r

sumInvariant :: (Show p, Num p, Eq p) => DTree p e -> Either String ()
sumInvariant Leaf = Right ()
sumInvariant _    = Right ()
-- Fails with floating point numbers, due to very small errors
--sumInvariant (DTree e p s c l r) 
--    | (s /= p + sumOf l + sumOf r) = Left $ "Sum mismatch:" ++ show [s,p,sumOf l,sumOf r]
--    | otherwise = (sumInvariant l) >> (sumInvariant r)

heapInvariant :: (Ord p, Num p) => DTree p e -> Either String ()
heapInvariant Leaf = Right ()
heapInvariant (DTree e p s c l r) 
    | (p < probOf l) = Left $ "Heap violation on left"
    | (p < probOf r) = Left $ "Heap violation on right"
    | otherwise = heapInvariant l >> heapInvariant r

zeroInvariant :: (Ord p, Num p) => DTree p e -> Either String ()
zeroInvariant Leaf = Right ()
zeroInvariant (DTree _ p _ c l r)
    | (p == 0) = Left $ "Zero value in tree"
    | otherwise = zeroInvariant l >> zeroInvariant r

memberInvariant :: (Eq p, Num p, Ord o) => Distribution p o -> Either String ()
memberInvariant _ = Right ()
-- Fails with floating numbers due to rounding error
--memberInvariant dist@(Distribution _ members _)
--    | reduce (toList dist) == members = Right ()
--    | otherwise = Left $ "Reduction doesn't match member map" 

-- | A series of tests on the internal structure of the distribution.
-- For debugging purposes.
invariants :: (Num p, Ord p, Show p, Ord e, Show e) => Distribution p e -> Either String ()
invariants dist@(Distribution tree members dups) = do
    sizeInvariant tree
    sumInvariant tree
    heapInvariant tree
    zeroInvariant tree
    memberInvariant dist
