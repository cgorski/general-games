{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Implement.Card.Example
  where

-- import Game.Implement.Card

-- data ExampleCardType = ExampleCardType Int String
--   deriving (Eq, Ord, Bounded)

-- instance Bounded ExampleCardType where
--   minBound = 1
--   maxBound = 4

-- instance Enum ExampleCardType where
--   toEnum n = ExampleCardType n (replicate n 'a')
--   fromEnum (ExampleCardType n c) = n

-- --data ExampleValueType = ValuePair Int Char deriving (Show, Eq)

-- data ExampleOrderingType = IntOrder | CharOrder

-- instance Card ExampleCardType where

-- instance OrderedCard ExampleCardType ExampleOrderingType where
--   compareCardBy IntOrder (ExampleCardType i1 _) (ExampleCardType i2 _) = i1 `compare` i2
--   compareCardBy CharOrder (ExampleCardType _ c1) (ExampleCardType _ c2) = c1 `compare` c2

-- instance CardCollection ExampleCardType where

-- deck :: [ExampleCardType]
-- deck = fullDeck

-- --deck2 :: [ExampleValueType]
-- --deck2 = map toValue deck

-- comp = compareCardBy CharOrder (deck !! 1) (deck !! 2)



