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
  ,newCpu
  ,genomeReplicate
  ,cpuReplicate
  ,iReadSelf
  ,cpuGenome
  ,score
  ,scoreCalc
  ,scoreCpu
  ,runProg
  )
  where

import Control.Monad.Random
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans (lift)
import Control.Monad.LoopWhile (loop,while)
import Control.Monad (when)
import System.Random.Shuffle (shuffleM)
import Data.List (nub, maximumBy, minimumBy, sortBy, foldl1', tails)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Int (Int64)

data Instruction =
  Nop | -- do nothing
  Zero | -- ax = 0
  Incr | -- ax = ax+1
  Neg | -- ax = -ax
  Add | -- ax = bx + cx
  Mult | -- ax = bx * cx
  Div | -- ax = bx / cx
  Mod | -- ax = bx mod cx
  TestLT | -- ax = 1 if  bx < cx else 0
  TestEQ | -- ax = 1 if bx == cx else 0
  Jmp | -- Add dx to instruction pointer mod self length if ax = 1
  NJmp | -- Add dx to instruction pointer mod self length if ax = 0
  IsStackAvail | -- ax = 1 if stack can be popped, otherwise 0
  Push | -- push ax to stack
  Pop | -- pop stack to ax, 0 if empty
  ReadInput | -- pop input stack to ax
  IsInputAvail | -- ax = 1 if len input stack > 0, else ax = 0
  IsMemAvail | -- ax = 1 if memory location bx available, otherwise ax = 0
  MemStore | -- Store ax in Memory Cell bx
  MemRetrieve | -- Retrieve memory cell bx to ax
  SwapBx | -- swap ax bx 
  SwapCx | -- swap ax cx 
  SwapDx | -- swap ax dx
  MateLength | -- The length of a mate genome to ax
  SelfLength | -- The length of a self genome to ax
  ReadMate | -- read instruction number bx mod len from mate and store type in ax
  ReadSelf | -- read instruction number bx mod len from self and store type in ax
  WriteChild | -- write instruction type ax to end of child
  WriteOutput  -- push number ax to output

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
  input :: [Int],
  output :: [Int],
  genome :: V.Vector Instruction,
  mateGenome :: V.Vector Instruction,
  childGenome :: [Instruction],
  executed :: [CPU],
  jmpSignal :: Maybe Int
} 

-- instance Show CPU where
--     show cpu = "ax: " ++ show (ax cpu) ++ " bx: " ++ show (bx cpu) ++ " cx: " ++ show (cx cpu) ++ " dx: " ++ show (dx cpu)
--                ++ " iPointer: " ++ show (iPointer cpu) ++ " iCounter: " ++ show (iCounter cpu) ++ " Instruc: " ++ show ((genome cpu) V.! (iPointer cpu))
--                ++ " child: " ++ show (reverse (childGenome cpu)) ++ "\n"
instance Show CPU where
    show cpu = "ax: " ++ show (ax cpu) ++ " bx: " ++ show (bx cpu) ++ " cx: " ++ show (cx cpu) ++ " dx: " ++ show (dx cpu)
               ++ " iPointer: " ++ show (iPointer cpu) ++ " iCounter: " ++ show (iCounter cpu) ++ " Instruc: " ++ show ((genome cpu) V.! (iPointer cpu))
               ++ " input: " ++ show (input cpu) ++ " output: " ++ show (output cpu) ++ "\n"
           
cpuReplicate :: CPU
cpuReplicate = newCpu {genome = genomeReplicate
                      ,mateGenome = genomeReplicate}

cpuGenome :: [Instruction] -> [Int] -> CPU
cpuGenome g i = newCpu {genome = V.fromList g,
                        mateGenome = V.fromList g,
                        input = i
                       }

cpuGenomeV :: V.Vector Instruction -> [Int] -> CPU
cpuGenomeV g i = cpuGenome (V.toList g) i

scoreCalc :: Int -> Int -> Double
scoreCalc i o = sqrt $ fromIntegral ((i-o)^(2 :: Int))
             
score :: [Int] -> [Int] -> Int
score inp out =
    if (length $ inp) /= (length $ out)
    then maxBound
    else ceiling $ sum $ zipWith scoreCalc inp out

scoreCpu :: CPU -> Int
scoreCpu cpu = score (input cpu) (reverse $ output cpu)

runGeneration :: RandomGen m => Int -> [CPU] -> WriterT String (Rand m) Int
runGeneration maxcount gen =
    do
    
      x <- getRandomR(1,2)
      tell "foo\n"
      tell "bar\n"
      return x

             
runProg :: IO ()
runProg =
    do
      (v,o) <- evalRandIO $ runWriterT $ runGeneration 10 [cpuReplicate]
      putStrLn $ show v
      putStrLn o
    

               
execute :: CPU -> Int -> CPU
execute cpu countmax 
    | (iCounter cpu) < countmax = execute (execInstruc cpu) countmax
    | otherwise = cpu


incrCpu :: CPU -> CPU
incrCpu cpu =
    let currentPointer = iPointer cpu
        genomeLength = length $ genome cpu
        newPointer = if genomeLength == 0 then 0 else (currentPointer + 1) `mod` genomeLength
        jmpInfo = case jmpSignal cpu of
                    Just newloc -> newloc
                    Nothing -> newPointer
    in
      cpu { iPointer = jmpInfo,
            executed = cpu:(executed cpu),
            iCounter = (iCounter cpu) + 1,
            jmpSignal = Nothing}

execInstruc :: CPU -> CPU
execInstruc cpu =
  let newc =
          case ((genome cpu) V.! (iPointer cpu)) of
            Nop -> iNop cpu
            Zero -> iZero cpu
            Incr -> iIncr cpu
            Neg -> iNeg cpu
            Add -> iAdd cpu
            Mult -> iMult cpu
            Div -> iDiv cpu
            Mod -> iMod cpu
            TestLT -> iTestLT cpu
            TestEQ -> iTestEQ cpu
            Jmp -> iJmp cpu
            NJmp -> iNJmp cpu
            IsStackAvail -> iIsStackAvail cpu
            Push -> iPush cpu
            Pop -> iPop cpu
            ReadInput -> iReadInput cpu
            IsInputAvail -> iIsInputAvail cpu
            IsMemAvail ->  iIsMemAvail cpu
            MemStore -> iMemStore cpu
            MemRetrieve -> iMemRetrieve cpu
            SwapBx -> iSwapBx cpu
            SwapCx -> iSwapCx cpu
            SwapDx -> iSwapDx cpu
            MateLength -> iMateLength cpu
            SelfLength -> iSelfLength cpu
            ReadMate -> iReadMate cpu
            ReadSelf -> iReadSelf cpu
            WriteChild -> iWriteChild cpu
            WriteOutput -> iWriteOutput cpu
  in
    incrCpu newc

newCpu = CPU { ax = 0,
               bx = 0,
               cx = 0,
               dx = 0,
               iCounter = 0,
               iPointer = 0,
               memory = M.fromList [],
               stack = [],
               input = [],
               output = [],
               genome = V.fromList [],
               mateGenome = V.fromList [],
               childGenome = [],
               executed = [],
               jmpSignal = Nothing}
                        
                   
genomeReplicate :: V.Vector Instruction            
genomeReplicate =
    V.fromList [
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          SwapCx, -- Move counter to Cx - cx = 9
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Neg,
          SwapDx, -- Move jump address to Dx - cx = 9 ; dx = -6
          ReadSelf, -- ax = Incr, cx = 9, dx = -6
          WriteChild,
          SwapBx, -- ax = 0, bx = Incr, cx = 9, dx = -6
          Incr,   -- ax = 1, bx = Incr, cx = 9, dx = -6
          SwapBx, -- ax = Incr, bx = 1, cx = 9, dx = -6
          TestLT, -- ax = 0, bx = 1, cx = 9, dx = -6
          Jmp, -- Jump back if  bx < cx - should be 10 runs
          Zero, -- Zero out all registers
          SwapBx,
          Zero,
          SwapCx,
          Zero,
          SwapDx,
          Zero,

          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          SwapBx,
          MateLength,
          SwapCx, -- Move counter to Cx - cx = 3
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Incr,
          Neg,
          SwapDx, -- Move jump address to Dx - cx = 3 ; dx = -6
          ReadMate, -- ax = Incr, cx = 3, dx = -6
          WriteChild,
          SwapBx, -- ax = 0, bx = Incr, cx = 3, dx = -6
          Incr,   -- ax = 1, bx = Incr, cx = 3, dx = -6
          SwapBx, -- ax = Incr, bx = 1, cx = 3, dx = -6
          TestLT, -- ax = 0, bx = 1, cx = 3, dx = -6
          Jmp, -- Jump back after bx = cx - should be 10 runs
          Zero, -- Zero out all registers
          SwapBx,
          Zero,
          SwapCx,
          Zero,
          SwapDx,
          Zero,

          Incr,
          WriteOutput,
          Incr,
          WriteOutput,
          Incr,
          WriteOutput,
          Incr,
          WriteOutput,
          Incr,
          WriteOutput,
          Zero,
              
          Incr,
          Neg,
          SwapDx,
          Nop,
          NJmp]
              
          
          

iNop :: CPU -> CPU
iNop cpu = cpu
                
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
    if (cx cpu) == 0 then cpu {ax = 0} else cpu { ax = (bx cpu) `div` (cx cpu) }

iMod :: CPU -> CPU
iMod cpu =
    if (cx cpu) == 0 then cpu {ax = 0} else cpu { ax = (bx cpu) `mod` (cx cpu) }

iTestLT :: CPU -> CPU
iTestLT cpu =
    case (bx cpu) < (cx cpu) of
      True -> cpu { ax = 1 }
      False -> cpu { ax = 0 }

iTestEQ :: CPU -> CPU
iTestEQ cpu =
    case (bx cpu) == (cx cpu) of
      True -> cpu { ax = 1 }
      False -> cpu { ax = 0 }
               
iJmp :: CPU -> CPU
iJmp cpu =
    if (ax cpu) == 1
    then if (length (genome cpu)) == 0 then cpu {iPointer = 0} else cpu { jmpSignal = Just (((iPointer cpu) + (dx cpu)) `mod` (length (genome cpu))) }
    else cpu

iNJmp :: CPU -> CPU
iNJmp cpu =
    if (ax cpu) == 0
    then if (length (genome cpu)) == 0 then cpu {iPointer = 0} else cpu { jmpSignal = Just (((iPointer cpu) + (dx cpu)) `mod` (length (genome cpu))) }
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
      (x:_) -> cpu { ax = x }

iReadInput :: CPU -> CPU
iReadInput cpu =
    case (input cpu) of
      [] -> cpu { ax = 0 }
      (x:_) -> cpu { ax = x }

iIsInputAvail :: CPU -> CPU
iIsInputAvail cpu =
    case (input cpu) of
      [] -> cpu { ax = 0 }
      _ -> cpu { ax = 1 }

iIsMemAvail :: CPU -> CPU
iIsMemAvail cpu =
    case M.member (bx cpu) (memory cpu) of
      True -> cpu { ax = 1 }
      False -> cpu { ax = 0 }

iMemStore :: CPU -> CPU
iMemStore cpu =
    cpu { memory = M.insert (bx cpu) (ax cpu) (memory cpu) }
               
iMemRetrieve :: CPU -> CPU
iMemRetrieve cpu =
    let val = M.lookup (bx cpu) (memory cpu) in
    case val of
      Just v -> cpu { ax = v }
      Nothing -> cpu { ax = 0 }

iSwapBx :: CPU -> CPU
iSwapBx cpu =
    let a = ax cpu
        b = bx cpu
    in cpu {ax = b,
            bx = a}

iSwapCx :: CPU -> CPU
iSwapCx cpu =
    let a = ax cpu
        c = cx cpu
    in cpu {ax = c,
            cx = a}

iSwapDx :: CPU -> CPU
iSwapDx cpu =
    let a = ax cpu
        d = dx cpu
    in cpu {ax = d,
            dx = a}

iReadMate :: CPU -> CPU
iReadMate cpu =
    let i = if (length (mateGenome cpu)) == 0 then Zero else (mateGenome cpu) V.! ((bx cpu) `mod` (length (mateGenome cpu)))
    in cpu {ax = fromEnum i}

iReadSelf :: CPU -> CPU
iReadSelf cpu =
    let i = if (length (genome cpu)) == 0 then Zero else (genome cpu) V.! ((bx cpu) `mod` (length (genome cpu)))
    in cpu {ax = fromEnum i}

iMateLength :: CPU -> CPU
iMateLength cpu =
    cpu { ax = V.length (mateGenome cpu) }

iSelfLength :: CPU -> CPU
iSelfLength cpu =
    cpu { ax = V.length (genome cpu) }

iWriteChild :: CPU -> CPU
iWriteChild cpu =
    cpu { childGenome = (toEnum (ax cpu)):(childGenome cpu) }
                                      
iWriteOutput :: CPU -> CPU
iWriteOutput cpu =
    cpu { output = (ax cpu):(output cpu) }


            

               


         
  

          
