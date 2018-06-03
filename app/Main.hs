{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Environment
import Game.Evolve.LinearCreature
import System.Console.CmdArgs


data Evolve = Evolve {numGenerations :: Int
                     ,numPerGeneration :: Int
                     ,maxExecCount :: Int}
              deriving (Data, Typeable, Show, Eq)

evolve :: Evolve
evolve = Evolve {numGenerations = def &= argPos 0 &= typ "NUM_GEN"
                ,numPerGeneration = def &= argPos 1 &= typ "NUM_PER_GEN"
                ,maxExecCount = def &= argPos 2 &= typ "MAX_EXEC_COUNT"}
                &= help "Run evolutionary algorithm"
main :: IO ()
main = do
  ev <- cmdArgs evolve
  mainProg (maxExecCount ev) (numPerGeneration ev) (numGenerations ev)
  



