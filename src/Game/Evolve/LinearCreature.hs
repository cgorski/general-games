{-# LANGUAGE OverloadedStrings #-}
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
  ,executeCpu
  ,newCpu
  ,genomeReplicate
--  ,cpuReplicate
  ,iReadSelf
  ,cpuGenome
  ,score
  ,scoreCalc
  ,scoreCpu
  ,runProg
  ,createInitGen
  ,preset
  ,scoreGeneric
  ,sfunc1
  ,sfunc2
  ,chooseByScore
  ,scoreAndChoose

  )
  where

import Control.Monad.Random
import Control.Monad.Writer (WriterT, runWriterT, tell, Writer)
import Control.Monad.Trans (lift,liftIO)
import Control.Monad.LoopWhile (loop,while)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad (when)
import qualified Control.Monad.Parallel as MP
import System.Random
import System.Random.Shuffle (shuffleM)
import Data.List (findIndex, splitAt, nub, maximumBy, minimumBy, sortBy, foldl1', tails)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Int (Int64)
import Text.Printf (printf)
import Data.Maybe (fromMaybe, fromJust)

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
  cpuid :: Maybe String,
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
  jmpSignal :: Maybe Int,
  parents :: Maybe (CPU,CPU)
} 

-- instance Show CPU where
--     show cpu = "ax: " ++ show (ax cpu) ++ " bx: " ++ show (bx cpu) ++ " cx: " ++ show (cx cpu) ++ " dx: " ++ show (dx cpu)
--                ++ " iPointer: " ++ show (iPointer cpu) ++ " iCounter: " ++ show (iCounter cpu) ++ " Instruc: " ++ show ((genome cpu) V.! (iPointer cpu))
--                ++ " child: " ++ show (reverse (childGenome cpu)) ++ "\n"
instance Show CPU where
    show cpu = "cpuid: " ++ fromMaybe "[No ID Provided" (cpuid cpu)
               ++ " ax: " ++ show (ax cpu) ++ " bx: " ++ show (bx cpu) ++ " cx: " ++ show (cx cpu) ++ " dx: " ++ show (dx cpu)
               ++ " iPointer: " ++ show (iPointer cpu) ++ " iCounter: " ++ show (iCounter cpu) -- ++ " Instruc: " ++ show ((genome cpu) V.! (iPointer cpu))
               ++ " input: " ++ show (input cpu) ++ " output: " ++ show (reverse $ output cpu) ++ "\n"
               ++ " genome: " ++ show (genome cpu) ++ " childGenome: " ++ show (reverse $ childGenome cpu) ++ "\n"

-- new funcs
-- func must return non-negative number
scoreGeneric :: (a -> Rational) -> [a] -> [(a, Double)]
scoreGeneric scoreFunc outputs =
  let
    rawScores = map (\out -> (out, scoreFunc out)) outputs
    minScore = minimum $ map (\(_,score) -> score) rawScores
    adjustedRawScores = map (\(out,score) -> (out,score-minScore)) rawScores
    fitnessSum = sum $ map (\(_,score) -> score) adjustedRawScores
    normalizedRawScores =
        if fitnessSum == 0
        then map (\(out,score) -> (out,(1/(fromIntegral $ length normalizedRawScores)))) adjustedRawScores
        else map (\(out,score) -> (out, if (score/fitnessSum) < (1/50) --minimum score
                                        then 1/50
                                        else score/fitnessSum)) adjustedRawScores
    sortedNormalized = sortBy (\(_,score1) (_,score2)-> score1 `compare` score2) normalizedRawScores
    accumulated = scanl1 (\(out1,score1) (out2,score2) -> (out2, score1+score2)) sortedNormalized
    maxAccumulated = maximum $ map (\(_,score) -> score) accumulated
    normalizedAccumulated = map (\(out,score) -> (out,score/maxAccumulated)) accumulated
    
  in
    reverse $ map (\(out,normalizedScore) -> (out,fromRational normalizedScore)) normalizedAccumulated

  

sfunc1 :: Int -> Rational
sfunc1 num = fromIntegral $ abs (10-(toInteger num))

sfunc2 :: Int -> Rational
sfunc2 num =
    let n = fromIntegral $ abs (10-(toInteger num)) + 1
    in
     if n == 0
     then 1
     else 1 / n


-- Take sorted list descending accumulated scores
chooseByScore :: RandomGen m => Int -> [(a, Double)] -> Rand m [(a, Double)]
chooseByScore numToChoose scoredPairs =
    let choosePair i =
            do
              (prob :: Double) <- getRandomR(0.0,1.0)
              pairIndex <-
                  case findIndex (\(_,score) -> prob > score) scoredPairs of
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
        

scoreAndChoose :: RandomGen m => (a -> Rational) -> Int -> [a] -> Rand m [(a, Double)]
scoreAndChoose scoreFunc numToChoose outputs = 
   chooseByScore numToChoose (scoreGeneric scoreFunc outputs)

          
              

   
  
 
--cpuReplicate :: [Int] -> String -> CPU
--cpuReplicate i cid = cpuGenome (V.toList genomeReplicate) i cid

cpuGenome :: [Instruction] -> [Instruction] -> [Int] -> String -> CPU
cpuGenome g mg i cid = newCpu {cpuid = Just cid,
                            genome = V.fromList g,
                            mateGenome = V.fromList mg,
                            input = i
                           }
leadz4 :: Int -> String
leadz4 n = printf "%04d" n

gcid :: Int -> Int -> String
gcid g c = (leadz4 g) ++ "--" ++ (leadz4 c)
                    
cpuGenomeV :: V.Vector Instruction -> [Int] -> String -> CPU
cpuGenomeV g i cid = cpuGenome (V.toList g) (V.toList g) i cid

scoreCalc :: Int -> Int -> Double
scoreCalc expected given = fromIntegral (
                                         let numcalc = (((expected-given)+1)^4)
                                         in
                                           if numcalc  > (maxBound :: Int)
                                           then (maxBound :: Int)
                                           else numcalc)


        
score :: [Int] -> [Int] -> Int
score expected given =
    let expectedlen = length expected
        (given2, _) = splitAt expectedlen given
        given2len = length given2
    in
      if given2len < expectedlen
      then maxBound
      else ceiling $ sum $ zipWith scoreCalc expected given

scoreCpu :: CPU -> Rational
scoreCpu cpu =
  let n = fromIntegral $ score (input cpu) (reverse $ output cpu)
  in
   if n == 0
   then 1
   else 1 / n

-- |
-- determines how many children will be duplicated based on score rank and total number of parents to score
-- the lower the scorerank, the better the score
-- start with trival example, use exponention or better later
-- survivalfactor is number of duplicates to create of creature
gradeFunc :: Int -> Int -> Int
gradeFunc scorerank numparents =
    let survivalfactor = numparents - scorerank
    in
      if scorerank < 3
      then survivalfactor * 5
      else if scorerank > 20
           then survivalfactor * 2
           else survivalfactor

gradePop :: [(CPU, Int)] -> [CPU]
gradePop scores =
    let lenscores = length scores
        rank :: [(CPU, Int)] -> [CPU] -> Int -> [CPU]
        rank [] ranked _ = ranked
        rank ((cpu,_):xs) ranked n = rank xs ((replicate (gradeFunc n lenscores) cpu)++ranked) (n+1)
    in
      rank scores [] 0
    
createInitGen :: Int -> [Instruction] -> [Int] -> [CPU]
createInitGen total g i =
    [cpuGenome g g i (gcid 0 n) | n <- [0..total-1]]

runGeneration2 :: RandomGen m => Int -> Int -> Double -> [CPU] -> RandT m IO [CPU]
runGeneration2  maxcount gennum mutateprob gencpus =
 
    let showcpuid :: CPU -> String
        showcpuid cpu = fromMaybe  "[No ID Provided]" $ cpuid cpu
        executec cpu = 
            do
              return $ executeCpu cpu maxcount

        sortScore :: (CPU, Rational) -> (CPU, Rational) -> Ordering
        sortScore (_,s1) (_,s2) = if s1 < s2
                                  then LT
                                  else
                                      if s1 > s2
                                      then GT
                                      else EQ

        sortScore2 :: (CPU, Double) -> (CPU, Double) -> Ordering
        sortScore2 (_,s1) (_,s2) = if s1 < s2
                                  then LT
                                  else
                                      if s1 > s2
                                      then GT
                                      else EQ
    in
      do

        lift $ putStrLn ("\n\n\n========================\nRunning generation number: " ++ (show gennum) ++ "\t" ++ "Size: " ++ (show $ length gencpus) ++ " Maxcount: " ++ (show maxcount))

        executed <- mapM executec gencpus
        (scores :: [(CPU, Rational)]) <- return $ sortBy sortScore $ map (\cpu -> (cpu, scoreCpu cpu)) executed
        lift $ putStrLn (show (drop ((length scores)-3) scores))
 
        newReplicated <- lift $ evalRandIO $ scoreAndChoose scoreCpu (length executed) executed
        lift $ putStrLn "\n\n\n\n\n"
        mutated <- mapM (\(cpu,score) ->
                             do
                               mutatedGenome <- lift $ evalRandIO $ mutate (childGenome cpu) mutateprob
                               return $ (cpu {childGenome = mutatedGenome}, score)
                        ) newReplicated
        newParents <- mapM (\(cpu,score) ->
                                do
                                  (newMate,_) <- lift $ evalRandIO $ chooseByScore1 mutated 
                                  return $ cpuGenome (reverse $ childGenome cpu) (V.toList $ mateGenome newMate) (input cpu) (fromJust $ cpuid cpu)
                      ) mutated
        return newParents
                                           

  
-- runGeneration :: RandomGen m => Int -> Int -> Double -> [CPU] -> WriterT [String] (Rand m) [CPU]
-- runGeneration maxcount gennum mutateprob gencpus =
--     let showcpuid :: CPU -> String
--         showcpuid cpu = fromMaybe  "[No ID Provided]" $ cpuid cpu
--         executec :: RandomGen m => CPU -> WriterT [String] (Rand m) CPU
--         executec cpu = 
--             do
--   --            tell ["Executing cpu ID: " ++ (showcpuid cpu)]
--               return $ executeCpu cpu maxcount

--         sortScore :: (CPU, Rational) -> (CPU, Rational) -> Ordering
--         sortScore (_,s1) (_,s2) = if s1 < s2
--                                   then LT
--                                   else
--                                       if s1 > s2
--                                       then GT
--                                       else EQ

--         sortScore2 :: (CPU, Double) -> (CPU, Double) -> Ordering
--         sortScore2 (_,s1) (_,s2) = if s1 < s2
--                                   then LT
--                                   else
--                                       if s1 > s2
--                                       then GT
--                                       else EQ

                                           
--     in
--       do
--         tell ["\n\n\n========================\nRunning generation number: " ++ (show gennum) ++ "\t" ++ "Size: " ++ (show $ length gencpus) ++ " Maxcount: " ++ (show maxcount)]
--         tell ["\n"]
--         executed <- mapM executec gencpus
--         (scores :: [(CPU, Rational)]) <- return $ sortBy sortScore $ map (\cpu -> (cpu, scoreCpu cpu)) executed
--         tell $ [show (drop ((length scores)-3) scores)]
--         tell $ ["\n"]
--         newReplicated <- lift $ scoreAndChoose scoreCpu (length executed) executed
--         tell $ ["\n\n\n\n\n"]
-- --        tell $ [show (drop 19 $ sortBy sortScore2 newReplicated)]
--         mutated <- mapM (\(cpu,score) ->
--                              do
--                                mutatedGenome <- lift $ mutate (childGenome cpu) mutateprob
--                                return $ (cpu {childGenome = mutatedGenome}, score)
--                         ) newReplicated
--         newParents <- mapM (\(cpu,score) ->
--                                 do
--                                   (newMate,_) <- lift $ chooseByScore1 mutated 
--                                   return $ cpuGenome (reverse $ childGenome cpu) (V.toList $ mateGenome newMate) (input cpu) (fromJust $ cpuid cpu)
--                       ) mutated
-- --        tell $ [show newParents]
--         return newParents

runProg :: Int -> Int -> Int -> [Instruction] -> Double -> [Int] -> IO ()
runProg maxexec gensize maxgen genome mutateprob expected =
    let
        loopTest :: (Int, [CPU], StdGen) -> Bool
        loopTest (gennum, _, _) =
            if gennum >= maxgen
            then True -- we have reached/exceeded the max number of generations
            else False -- keep going

        initialGen = createInitGen gensize genome expected
                         
        progLoop :: (Int, [CPU], StdGen) -> IO (Int,[CPU],StdGen)
        progLoop (gennum, prevgen, randgen) =
            do
              (v,randgen2) <- runRandT (runGeneration2 maxexec gennum mutateprob prevgen) randgen
              return (gennum+1,v,randgen2)
    in
      do
        randGen <- getStdGen
        (gennum, final, _) <- iterateUntilM loopTest progLoop (0,initialGen, randGen)
        putStrLn $ show gennum

preset = runProg 2000 1000 10000000 (V.toList genomeReplicate) 0.001 [1,2,4,8,16,32,64,128]                 
      


            
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
                                   2 -> return [i, rins] -- Insert
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
          n -> executeCpu (execInstruc cpu) countmax
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

newCpu = CPU { cpuid = Nothing,
               ax = 0,
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
               jmpSignal = Nothing,
               parents = Nothing }
                        
                   
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


            

               


         
  

          
