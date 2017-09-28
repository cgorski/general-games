{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Game.Implement.Card
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Implement.Card module provides fundamental operations for a deck of cards.
module Game.Evolve.LinearCreature
  (
   CPU(..)
  ,Instruction(..)
  ,execute
   
  )
  where

import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import Data.List (nub, maximumBy, minimumBy, sortBy, foldl1', tails)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Int (Int64)

data Instruction =
  Zero | -- ax = 0
  Incr | -- ax = ax+1
  Neg | -- ax = -ax
  Add | -- ax = bx + cx
  Mult | -- ax = bx * cx
  Div | -- ax = bx / cx
  Mod | -- ax = bx mod cx
  TestGT | -- ax = 1 if  bx > cx else 0
  TestEQ | -- ax = 1 if bx == cx else 0
  Jmp | -- Add bx to instruction pointer mod self length if ax = 1
  IsStackAvail | -- ax = 1 if stack can be popped, otherwise 0
  Push | -- push ax to stack
  Pop | -- pop stack to ax, 0 if empty
  ReadInput | -- pop input stack to ax
  IsInputAvail | -- ax = 1 if len input stack > 0, else ax = 0
  Store | -- Store ax in Memory Cell bx
  Retrieve | -- Retrieve memory cell bx to ax
  SwapBx | -- swap ax bx 
  SwapCx | -- swap ax cx 
  SwapDx | -- swap ax dx
  IsMateAvail | -- ax = 1 if instruction number bx exists in mate
  IsSelfAvail | -- ax = 1 if instruction number bx exists in self
  ReadMate | -- read instruction number bx from mate and store value in ax
  ReadSelf | -- read instruction number bx from self and store value in ax
  WriteChild | -- write instruction ax to end of child
  WriteResult  -- push number ax to result list

  deriving (Show, Enum, Eq, Ord, Bounded)

  
data CPU = CPU {
  ax :: Int,
  bx :: Int,
  cx :: Int,
  dx :: Int,
  iCounter :: Int,
  iPointer :: Int,
  memory :: M.Map Int Int,
  stack :: [Int],
  genome :: V.Vector Instruction,
  mateGenome :: V.Vector Instruction,
  childGenome :: V.Vector Instruction
}

execute :: CPU -> Int -> CPU
execute cpu countmax =
  if (iCounter cpu) >= countmax
  then cpu
  else execute (execInstruc cpu) countmax

incrCpu :: CPU -> CPU
incrCpu cpu =
    let currentPointer = iPointer cpu
        genomeLength = length $ genome cpu
        newPointer = (currentPointer + 1) `mod` genomeLength
    in
      cpu { iPointer = newPointer}

execInstruc :: CPU -> CPU
execInstruc cpu =
  let newCpu =
          case ((genome cpu) V.! (iPointer cpu)) of
            Zero -> iZero cpu
            Incr -> iIncr cpu
            Neg -> iNeg cpu
            Add -> iAdd cpu
            Mult -> iMult cpu
            Div -> iDiv cpu
            Mod -> iMod cpu
            Jmp -> iJmp cpu
            IsStackAvail -> iIsStackAvail cpu
            Push -> iPush cpu
            Pop -> iPop cpu
            -- ReadInput -> iPopInput cpu
            -- Store -> iStore cpu
            -- Retrieve -> iRetrieve cpu
            -- SwapBx -> iSwapBx cpu
            -- SwapCx -> iSwapCx cpu
            -- SwapDx -> iSwapDx cpu
            -- ReadMate -> iReadMate cpu
            -- ReadSelf -> iReadSelf cpu
            -- WriteChild -> iWriteChild cpu
            -- WriteResult -> iWriteResult cpu
  in
    incrCpu newCpu 

iZero :: CPU -> CPU
iZero cpu =
    cpu { ax = 0 }

iIncr :: CPU -> CPU
iIncr cpu =
    cpu { ax = (ax cpu) + 1 }

iNeg :: CPU -> CPU
iNeg cpu =
    cpu { ax = (-(ax cpu)) }
     
iAdd :: CPU -> CPU
iAdd cpu =
    cpu { ax = (bx cpu) + (cx cpu) }

iMult :: CPU -> CPU
iMult cpu =
    cpu { ax = (bx cpu) * (cx cpu) }

iDiv :: CPU -> CPU
iDiv cpu =
    cpu { ax = (bx cpu) `div` (cx cpu) }

iMod :: CPU -> CPU
iMod cpu =
    cpu { ax = (bx cpu) `mod` (cx cpu) }

iJmp :: CPU -> CPU
iJmp cpu =
    if (ax cpu) == 1
    then cpu { iPointer = (iPointer cpu) + ((bx cpu) `mod` (cx cpu)) }
    else cpu

iIsStackAvail :: CPU -> CPU
iIsStackAvail cpu =
    case (stack cpu) of
      [] -> cpu { ax = 0 }
      _ -> cpu { ax = 1 }

iPush :: CPU -> CPU
iPush cpu =
    cpu { stack = (ax cpu):(stack cpu) }
         
iPop :: CPU -> CPU
iPop cpu =
    case (stack cpu) of
      [] -> cpu { ax = 0 }



         
  

          
