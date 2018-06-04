{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


-- |
-- Module      : Game.Implement.Card
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Implement.Card module provides fundamental operations for a deck of cards.
module Game.Evolve.LinearCreature
  (
--   CPU(..)
--  ,Instruction(..)
  mainProg

  )
  where

import Control.Monad.Random
import Control.Parallel.Strategies
import Data.List 
import qualified Data.Vector as V
import Text.Printf
import System.IO
import qualified Data.Distribution as D
import qualified Data.Distribution.Core as DC
import qualified Data.Distribution.Sample as DS
import Data.Bits
import Data.UUID
import qualified Game.Evolve.Distribution as GED
import qualified Control.Foldl as F

data Instruction =
  Nop | -- do nothing
  Zero | -- ax = 0
  Incr | -- ax = ax+1
  Neg | -- ax = -ax
  Add | -- ax = bx + cx
  Mult | -- ax = bx * cx
  Div | -- ax = bx / cx
  Mod | -- ax = bx mod cx
  RotateL | -- ax = bx rotatel cx
  TestLT | -- ax = 1 if  bx < cx else 0
  TestEQ | -- ax = 1 if bx == cx else 0
  Jmp | -- Add dx to instruction pointer mod self length if ax = 1
  NJmp | -- Add dx to instruction pointer mod self length if ax = 0
  IsStackAvail | -- ax = 1 if stack can be popped, otherwise 0
  Push | -- push ax to stack
  Pop | -- pop stack to ax, 0 if empty
  IsStackAvail2 | -- ax = 1 if stack can be popped, otherwise 0
  Push2 | -- push ax to stack
  Pop2 | -- pop stack to ax, 0 if empty
  ReadInput | -- pop input stack to ax
  IsInputAvail | -- ax = 1 if len input stack > 0, else ax = 0
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
  cpuid :: UUID,
  ax :: Int,
  bx :: Int,
  cx :: Int,
  dx :: Int,
  iCounter :: Int,
  iPointer :: Int,
  stack :: [Int],
  stack2 :: [Int],
  input :: [Int],
  output :: [Int],
  genome :: V.Vector Instruction,
  mateGenome :: V.Vector Instruction,
  childGenome :: [Instruction],
  jmpSignal :: Maybe Int,
  parents :: Maybe (CPU,CPU)
} deriving (Show)

instance Eq CPU where
  c1 == c2 = (cpuid c1) == (cpuid c2)

instance Ord CPU where
  c1 `compare` c2 = (cpuid c1) `compare` (cpuid c2)


scoreGeneric :: (a -> Rational) -> [a] -> [(a, Double)]
scoreGeneric scoreFunc outputs =
  let
    rawScores = map (\out -> (out, scoreFunc out)) outputs
    minScore = minimum $ map (\(_,score1) -> score1) rawScores
    adjustedRawScores = map (\(out,score1) -> (out,score1-minScore)) rawScores
    fitnessSum = sum $ map (\(_,score1) -> score1) adjustedRawScores

    normalizedRawScores =
        if fitnessSum == 0
        then map (\(out,_) -> (out,(1/(fromIntegral $ length normalizedRawScores)))) adjustedRawScores
        else map (\(out,score1) -> (out, if (score1/fitnessSum) < (1/20) --minimum score
                                        then 1/20
                                        else score1/fitnessSum)) adjustedRawScores
    sortedNormalized = sortBy (\(_,score1) (_,score2)-> score1 `compare` score2) normalizedRawScores
    accumulated = scanl1 (\(_,score1) (out2,score2) -> (out2, score1+score2)) sortedNormalized
    maxAccumulated = maximum $ map (\(_,score1) -> score1) accumulated
    normalizedAccumulated = map (\(out,score1) -> (out,score1/maxAccumulated)) accumulated
    
  in
    reverse $ map (\(out,normalizedScore) -> (out,fromRational normalizedScore)) normalizedAccumulated

-- scoreGenericD :: (a -> Rational) -> [a] -> [(a, Rational)]
-- scoreGenericD scoreFunc outputs =
--   let
--     rawScores = map (\out -> (out, scoreFunc out)) outputs
--     minScore = minimum $! map (\(_,score1) -> score1) rawScores
--     adjustedRawScores = map (\(out,score1) -> (out,score1-minScore)) rawScores
--     fitnessSum = foldl' (+) 0 $! map (\(_,score1) -> score1) adjustedRawScores
--     normalizedRawScores =
--         if fitnessSum == 0
--         then map (\(out,_) -> (out,(1/(fromIntegral $ length rawScores)))) adjustedRawScores
--         else map (\(out,score1) -> (out, if (score1/fitnessSum) < (1/20) --minimum score
--                                         then 1/20
--                                         else score1/fitnessSum)) adjustedRawScores
--   in
--     normalizedRawScores

scoreGenericD :: (a -> Rational) -> [a] -> [(a, Rational)]
scoreGenericD scoreFunc outputs =
  let
    rawScores = map (\out -> (out, (scoreFunc out))) outputs
    totalFitness = foldl' (+) 0 $ map (\(_,s) -> s) rawScores
  in
    map (\(o,s) -> (o, min (s/totalFitness) (1/20))) rawScores
                      
  -- in
  --   map (\(out,score1) -> (out, if (score1/totalFitness) < (1/20) --minimum score
  --                               then 1/20
  --                               else score1/totalFitness)) scores




-- Take sorted list descending accumulated scores
chooseByScore :: RandomGen m => Int -> [(a, Double)] -> Rand m [(a, Double)]
chooseByScore !numToChoose !scoredPairs =
    let choosePair _ =
            do
              (prob :: Double) <- getRandomR(0.0,1.0)
              pairIndex <-
                  case findIndex (\(_,score1) -> prob > score1) scoredPairs of
                    Just index ->
                      case index of
                        0 -> return index
                        n -> return $ n - 1
                    Nothing -> return $ length scoredPairs - 1
              return $ scoredPairs !! pairIndex
    in
      do
        mapM choosePair [1..numToChoose]

chooseByScore1 :: RandomGen m => [(a, Double)] -> Rand m (a, Double)
chooseByScore1 scoredPairs =
    do
      chosen <- chooseByScore 1 scoredPairs
      return $ chosen !! 0

chooseByScore1D :: (Ord a, RandomGen m) => DS.Generator (a, Rational) -> Rand m (a, Rational)
chooseByScore1D scoredPairs =
    do
      chosen <- chooseByScoreD_ 1 scoredPairs
      return $ chosen !! 0

chooseByScoreD :: (Ord a, RandomGen m) => Int -> [(a, Rational)] -> Rand m [(a, Rational)]
chooseByScoreD numToChoose scoredPairs =
  let gen = GED.fromList (map (\(a,s) -> ((a,s),((fromRational s) :: Double))) scoredPairs)
  in
    replicateM numToChoose $! GED.sample gen
                           

makeGen :: (Ord a) => [(a, Rational)] -> DS.Generator (a, Rational)
makeGen scoredPairs =
  let distrib = D.fromList (map (\(a,s) -> ((a,s),s) ) scoredPairs)
  in
    DS.fromDistribution distrib



chooseByScoreD_ :: (Ord a, RandomGen m) => Int -> DS.Generator (a, Rational) -> Rand m [(a, Rational)]
chooseByScoreD_ numToChoose gen =
  replicateM numToChoose (do
                             (a,s) <- DS.getSample gen
                             return $ (a, s))


  

scoreAndChoose :: RandomGen m => (a -> Rational) -> Int -> [a] -> Rand m [(a, Double)]
scoreAndChoose scoreFunc numToChoose outputs = 
   chooseByScore numToChoose (scoreGeneric scoreFunc outputs)

scoreAndChooseD :: (Ord a, RandomGen m) => (a -> Rational) -> Int -> [a] -> Rand m [(a, Rational)]
scoreAndChooseD scoreFunc numToChoose outputs = 
   chooseByScoreD numToChoose (scoreGenericD scoreFunc outputs)


 

cpuGenome :: RandomGen m => [Instruction] -> [Instruction] -> [Int] -> Rand m CPU
cpuGenome g mg i =
  do
    uuid <- getRandom
    return $ CPU {
      ax = 0,
      bx = 0,
      cx = 0,
      dx = 0,
      iCounter = 0,
      iPointer = 0,
      stack = [],
      stack2 = [],               
      input = i,
      output = [],

      cpuid = uuid,
      genome = V.fromList g,
      mateGenome = V.fromList mg,
      childGenome = [],


      jmpSignal = Nothing,
      parents = Nothing}



         
      
leadz8 :: Int -> String
leadz8 n = printf "%08d" n

                    
scoreCalc :: Int -> Int -> Integer
scoreCalc expected given =
  let (numcalc ::Integer) = ((((toInteger expected)-(toInteger given))+1)^(2 :: Int))
  in
    if numcalc  > (toInteger (maxBound :: Int))
    then toInteger $ (maxBound :: Int)-(2^(32 :: Int))
    else toInteger $ numcalc


        
score :: [Int] -> [Int] -> Int
score expected given =
    let expectedlen = length expected
        (given2, _) = splitAt expectedlen given
        given2len = length given2
    in
      if given2len < expectedlen
      then maxBound-1
      else
        let (sCalcs :: [Integer]) = zipWith scoreCalc expected given
            (total :: Integer) = sum sCalcs
            newTotal = if total > (toInteger (maxBound :: Int))
                       then (toInteger (maxBound :: Int))-(2^(32 :: Int))
                       else total
        in
          fromIntegral newTotal
          
scoreCpu :: CPU -> Rational
scoreCpu cpu =
  let n = fromIntegral $ score (input cpu) (reverse $ output cpu)
  in
   if n == 0
   then 1
   else 1 / n



    

createInitGen :: RandomGen m => Int -> [Instruction] -> Rand m [Int] -> Rand m [CPU]
createInitGen total g i =
  let 
    mkOrg _ =
      do
        randInput1 <- i
        cpuGenome g g randInput1
  in
    mapM mkOrg [1..total]
  

        
                                           

  


data Generation = Generation {
  genNum :: Int,
  cpus :: [CPU]
  }
    deriving (Show)

newGeneration :: Int -> [CPU] -> Generation
newGeneration gn cs=
  Generation {
         genNum = gn,
         cpus = cs
         }

         
         

mainProg_ :: Int -> Int -> Int -> [CPU] -> Rand StdGen [Int] -> Double -> IO ()
mainProg_ maxexec currentGen maxGen lastGen inputFunc mutateProb =
  let
    parMapChunk f = withStrategy (parListChunk 1 rseq) . map f
  in
    do
      executedGen <- return $ parMapChunk (\cpu -> (executeCpu cpu maxexec)) lastGen
--      generation <- return $ newGeneration currentGen executedGen
      bestCPUs <- return $ (sortBy (\(_,s1) (_,s2) -> s2 `compare` s1) $ map (\cpu -> (cpu, scoreCpu cpu)) executedGen)
      
--      bestCPUGen <- return $ (bestCPU,currentGen)
      
--      putStrLn $ (show generation) ++ "\n\n\n\n=========\n\n\n\n"
--      hFlush stdout

--      putStrLn "\n\n\n\nBEST\n\n\n\n"
--      r <- return $ scoreGenericD 

      scoredGen <- evalRandIO $ scoreAndChooseD scoreCpu (length executedGen) executedGen
      mutated <- mapM (\(cpu,score1) ->
                         do
                           mutatedGenome <- evalRandIO $ mutate (childGenome cpu) mutateProb
                           return $ (cpu {childGenome = mutatedGenome}, score1)
                      ) scoredGen
      scoredGenGen <- return $ makeGen scoredGen
      nextGen <- mapM (\(cpu,_) ->
                         do
                           (newMate,_) <- evalRandIO $ chooseByScore1D scoredGenGen
                           newInput <- evalRandIO $ inputFunc
--                           putStrLn $ show $ (reverse $ childGenome cpu)
                           evalRandIO $ cpuGenome (reverse $ childGenome cpu) (V.toList $ mateGenome newMate) newInput
                      ) mutated

--      putStrLn $ show nextGen

      putStrLn "\n\n\n\nBEST\n\n\n\n"
      putStrLn $ "Current Generation: " ++ (leadz8 currentGen)
      putStrLn ""
      putStrLn $ show $ bestCPUs !! 2 
      putStrLn ""
      putStrLn $ show $ bestCPUs !! 1
      putStrLn ""
      putStrLn $ show $ bestCPUs !! 0

      if currentGen < (maxGen-1)
      then mainProg_ maxexec (currentGen+1) maxGen nextGen inputFunc mutateProb
      else return $ ()
    

--mainProg :: Int -> Int -> [Instruction] -> IO ()
mainProg :: Int -> Int -> Int -> Double -> IO ()
mainProg maxExecCount numPerGeneration numGenerations mutateProb =
  do
    initGen <- evalRandIO $ createInitGen numPerGeneration (V.toList genomeReplicate) randInput 
    mainProg_ maxExecCount 0 numGenerations initGen randInput mutateProb

      
randInput :: RandomGen m => Rand m [Int]
randInput =
  do
    start <- getRandomR(1,10000)
    return $ [start*(2^n) |(n :: Int) <- [10..19]]

            
mutate :: RandomGen m => [Instruction] -> Double -> Rand m [Instruction]
mutate ins probthres =

    let minI = minBound :: Instruction
        maxI = maxBound :: Instruction
        getrandins :: RandomGen m => Rand m Instruction
        getrandins =
            do 
              (rint :: Int) <- getRandomR(fromEnum minI, fromEnum maxI)
              return $ toEnum rint
                     
        mut :: RandomGen m => Double -> Instruction -> Rand m [Instruction]
        mut mutthres i =
            do
              (mutrand :: Double) <- getRandomR(0.0,1.0)
              (randmuttype :: Int) <- getRandomR(0,2)
              rins <- getrandins
              mutatedIns <- if mutrand > mutthres
                            then return [i]  -- random value greater than probability threshold for mutation - do nothing
                            else case randmuttype of
                                   0 -> return []  -- Delete
                                   1 -> return [rins] -- Mutate
                                   _ -> return [i, rins] -- Insert
              return mutatedIns
        
      

    in
      do
        mutated <-
            case ins of
              [] -> return [[]]
              ns -> mapM (mut probthres) ns
        return $ concat mutated
              
            
            
executeCpu :: CPU -> Int -> CPU
executeCpu cpu countmax
    | (iCounter cpu) < countmax =
        case V.length (genome cpu) of
          0 -> cpu {iCounter = countmax}
          _ -> executeCpu (execInstruc cpu) countmax
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
      cpu { iPointer = abs jmpInfo,
            iCounter = (iCounter cpu) + 1,
            jmpSignal = Nothing}

execInstruc :: CPU -> CPU
execInstruc !cpu =
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
            RotateL -> iRotateL cpu
            TestLT -> iTestLT cpu
            TestEQ -> iTestEQ cpu
            Jmp -> iJmp cpu
            NJmp -> iNJmp cpu
            IsStackAvail -> iIsStackAvail cpu
            Push -> iPush cpu
            Pop -> iPop cpu
            IsStackAvail2 -> iIsStackAvail2 cpu
            Push2 -> iPush2 cpu
            Pop2 -> iPop2 cpu
            ReadInput -> iReadInput cpu
            IsInputAvail -> iIsInputAvail cpu
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

          ReadInput,
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
    if (cx cpu) == 0 || (bx cpu) == minBound then cpu {ax = 0} else cpu { ax = (bx cpu) `div` (cx cpu) }

iMod :: CPU -> CPU
iMod cpu =
    if (cx cpu) == 0 then cpu {ax = 0} else cpu { ax = (bx cpu) `mod` (cx cpu) }

iRotateL :: CPU -> CPU
iRotateL cpu =
    if (cx cpu) == 0 then cpu {ax = 0} else cpu { ax = (bx cpu) `rotate` (cx cpu) }

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

iIsStackAvail2 :: CPU -> CPU
iIsStackAvail2 cpu =
    case (stack2 cpu) of
      [] -> cpu { ax = 0 }
      _ -> cpu { ax = 1 }

iPush2 :: CPU -> CPU
iPush2 cpu =
    cpu { stack2 = (ax cpu):(stack2 cpu) }
         
iPop2 :: CPU -> CPU
iPop2 cpu =
    case (stack2 cpu) of
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
    let
        minI = minBound :: Instruction
        maxI = maxBound :: Instruction
        minint = fromEnum minI :: Int
        maxint = fromEnum maxI :: Int 
    in
      if ax cpu < minint || ax cpu > maxint
      then cpu { childGenome = (toEnum 0):(childGenome cpu) }
      else cpu { childGenome = (toEnum (ax cpu)):(childGenome cpu) }

                                      
iWriteOutput :: CPU -> CPU
iWriteOutput cpu =
    cpu { output = (ax cpu):(output cpu) }


            

               


         
  

          
