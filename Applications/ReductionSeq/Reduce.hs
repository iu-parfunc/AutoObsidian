{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where

-- Obsidian
import Obsidian hiding (tail, take)
import Obsidian.CodeGen.CUDA
import Obsidian.Run.CUDA.Exec hiding (exec)

-- Autotuning
import Auto.BitClimbSearch           as BS
import Auto.ExhaustiveSearch         as ES
import Auto.GeneticSearch            as GS
import Auto.RandomSearch             as RS
import Auto.SimulatedAnnealingSearch as SA
import Auto.ResultLog hiding (push)
import Auto.SearchMonad

-- Timing
import Data.Time.Clock

-- System
import System.Environment
import System.IO 

import Data.Word
import Data.List hiding (zipWith)
import Control.Monad.State 

import Prelude hiding (map,zipWith,sum,replicate,take,drop,iterate)
import qualified Data.Vector.Storable as V

-----------------------------------------------------------------
-- Tuning related 
-----------------------------------------------------------------
data Result = Result ([Int],Double)

instance Annealable Result where
  extract (Result (_,d)) = d

instance Eq Result where
  (Result (_,d1)) == (Result (_,d2)) = d1 == d2

instance Ord Result where
  compare (Result (_,d1)) (Result (_,d2)) = compare d1 d2

instance Show Result where
  show (Result (p,r)) = show p ++ " | " ++ show r

instance CSV Result where
  toCSVRow (Result (xs, d)) =
    unwords (intersperse "," (map show xs)) ++ "," ++ show d


-----------------------------------------------------------------
-- Reduction
-----------------------------------------------------------------
reduceKernel :: (Compute t, Data a) 
                => (a -> a -> a)
                -> Pull Word32 a 
                -> Program t (SPush t a)
reduceKernel f arr 
  | len arr == 1 = return $ push arr
  | otherwise =
    do let (a1,a2) = halve arr
       arr' <- compute $ zipWith f a1 a2
       reduceKernel f arr'

block_reduce :: Data a =>  (a -> a -> a) -> Pull Word32 a -> Program Block (Push Block Word32 a)
block_reduce = reduceKernel

thread_reduce :: Data a =>  (a -> a -> a) -> Pull Word32 a -> Program Thread (Push Thread Word32 a)
thread_reduce = reduceKernel 


hybrid_reduce :: Data a
              => Word32 
              -> (a -> a -> a)
              -> Pull Word32 a -> Push Block Word32 a
hybrid_reduce seq_th f arr = exec $ b_body arr
  where
    b_body arr = do
      arr' <- compute 
              $ asBlockMap (exec . thread_reduce f)
              $ splitUp seq_th arr
      reduceKernel f arr' 



reductions2 :: forall a . Data a
           => Word32 
           -> (a -> a -> a)
           -> Pull EWord32 (SPull a)
           -> Push Grid EWord32 a 
reductions2 seq_th f arr = asGridMap (hybrid_reduce seq_th f) arr

-----------------------------------------------------------------
--
----------------------------------------------------------------- 
genIt :: (ToProgram (Pull EWord32 a -> Push Grid EWord32 a), Data a)
         => String
         -> (Pull EWord32 (SPull a) -> Push Grid EWord32 a)
         -> Word32
         -> Word32
         -> IO () 
genIt name kernel threads size =
  putStrLn $
  genKernel threads name  
    (kernel . splitUp size) 


testIt = do
  genIt "reduce" (reductions2 32  (+) :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32) 128 128

  putStrLn "VARYING SEQ_TH (threads:128)" 
  execIt (reductions2 32 (+)) 1024 2 128 2
  execIt (reductions2 64 (+)) 1024 2 128 2
  execIt (reductions2 128 (+)) 1024 2 128 2
  execIt (reductions2 256 (+)) 1024 2 128 2
  execIt (reductions2 512 (+)) 1024 2 128 2
  execIt (reductions2 1024 (+)) 1024 2 128 2
  
  putStrLn "OTHER THREADS SETTING (256)" 
  execIt (reductions2 32 (+)) 1024 2 256 2
  execIt (reductions2 64 (+)) 1024 2 256 2
  execIt (reductions2 128 (+)) 1024 2 256 2
  execIt (reductions2 256 (+)) 1024 2 256 2
  execIt (reductions2 512 (+)) 1024 2 256 2
  execIt (reductions2 1024 (+)) 1024 2 256 2

  putStrLn "OTHER THREADS SETTING (512)" 
  execIt (reductions2 32 (+)) 1024 2 512 2
  execIt (reductions2 64 (+)) 1024 2 512 2
  execIt (reductions2 128 (+)) 1024 2 512 2
  execIt (reductions2 256 (+)) 1024 2 512 2
  execIt (reductions2 512 (+)) 1024 2 512 2
  execIt (reductions2 1024 (+)) 1024 2 512 2

  putStrLn "STRANGE WARP SETTINGS (that does not properly divide size)"
  execIt (reductions2 37 (+)) 1024 2 256 2
  execIt (reductions2 61 (+)) 1024 2 256 2
  execIt (reductions2 165 (+)) 1024 2 256 2
  execIt (reductions2 294 (+)) 1024 2 256 2
  execIt (reductions2 458 (+)) 1024 2 256 2
  execIt (reductions2 759 (+)) 1024 2 256 2

  putStrLn "STRANGE WARP SETTINGS (that are more \"nice\")"
  execIt (reductions2 1 (+)) 1024 2 256 2
  execIt (reductions2 2 (+)) 1024 2 256 2
  execIt (reductions2 4 (+)) 1024 2 256 2
  execIt (reductions2 8 (+)) 1024 2 256 2
  execIt (reductions2 16 (+)) 1024 2 256 2
  execIt (reductions2 32 (+)) 1024 2 256 2

  
--main = testIt
-----------------------------------------------------------------
-- Execute it
-----------------------------------------------------------------
execIt kernel chunk_size n_chunks threads blocks =
  withCUDA $
    do
      kern <- capture threads ( (kernel . splitUp chunk_size) :: Pull EWord32 EWord32 -> Push Grid EWord32 EWord32)

      useVector (V.fromList [0..(chunk_size * (fromIntegral n_chunks))::Word32]) $ \input ->
        withVector n_chunks $ \output ->
        do
          output <== (blocks, kern) <> input
          res <- peekCUDAVector output
          lift $ putStrLn $ show res 
                  
                              
      

-----------------------------------------------------------------
-- TUNING
-----------------------------------------------------------------
popCount, bitCount, generations1, generations2, iterations1, iterations2 :: Int
popCount = 5
bitCount = 5 -- (2^32) (0..31)
generations1 = 4
generations2 = 10
iterations1 = 10
iterations2 = popCount * generations2

main = do

  args <- getArgs

  res <- case args of
    ("RANDOM":_) -> random (tail args)
    ("BITCLIMB":_) -> bitclimb (tail args)
    ("EXHAUSTIVE":_) -> exhaustive (tail args)
    ("SGA":_) -> genetic (tail args)
    ("SA":_) -> anneal (tail args)    
    _ -> exhaustive []

  let filename = argsToFileName args
  let (b, Just a) = resultCSV res
  writeFile filename $ a

  let resultOverTime = unlines
        $ map (\(iter,Result p) -> show iter ++ ", " ++ show (snd p))
                                 (resultLogBestOverTime res)
  writeFile ("timeseries"++filename) resultOverTime


  where
    argsToFileName [] = "reduceSeq_EXHAUSTIVE_SEQTH.csv"
    argsToFileName [x] = "recudeSeq_" ++ x ++ "_SEQTH.csv"
    argsToFileName [x,y] = "reduceSeq_" ++ x ++ "_" ++ y ++ ".csv"

    exhaustive args = do
      putStrLn "Exhaustive search"
      case args of
        [] ->
          ES.runSearch (ES.Config [[1..12]])
                       (prog1 :: ExhaustiveSearch Result (Maybe Result)) 
        ["SEQTH"] -> 
          ES.runSearch (ES.Config [[1..12]])
                       (prog1 :: ExhaustiveSearch Result (Maybe Result))
        ["BOTH"] ->
          ES.runSearch (ES.Config [ [1..12]
                                  , [0..31]])
                       (prog2 :: ExhaustiveSearch Result (Maybe Result))
          
    random args = do
      putStrLn "Random search"
      case args of
        [] ->
          RS.runSearch (RS.Config [(0,10)] iterations1)
                       (prog1 :: RandomSearch Result (Maybe Result))
        ["SEQTH"] ->
          RS.runSearch (RS.Config [(0,10)] iterations1)
                       (prog1 :: RandomSearch Result (Maybe Result))
        ["BOTH"]    ->
          RS.runSearch (RS.Config [(0,10),(0,31)] iterations2)
                       (prog2 :: RandomSearch Result (Maybe Result))     


    -- bitcount here needs to be enough for both params..
    -- we should have a way to specify different bit counts
    bitclimb args = do
      putStrLn "Bit climb search"
      case args of
        [] ->
          BS.runSearch (BS.Config bitCount 1 iterations1 1 True)
                       (prog1 :: BitClimbSearch Result (Maybe Result))
        ["SEQTH"] ->
          BS.runSearch (BS.Config bitCount 1 iterations1 1 True)
                       (prog1 :: BitClimbSearch Result (Maybe Result))
  
        ["BOTH"]    ->
          BS.runSearch (BS.Config bitCount 2 iterations2 1 True)
                       (prog2 :: BitClimbSearch Result (Maybe Result))

    
    genetic args = do
      putStrLn "Simple genetic algorithm"
      case args of
        [] ->
          GS.runSearch (GS.Config bitCount 1 popCount generations1 0.2 3 1 True)
                       (prog1 :: GeneticSearch Result (Maybe Result))
        ["SEQTH"] ->
          GS.runSearch (GS.Config bitCount 1 popCount generations1 0.2 3 1 True)
                       (prog1 :: GeneticSearch Result (Maybe Result))
        ["BOTH"]    ->
          GS.runSearch (GS.Config bitCount 2 popCount generations2 0.2 3 1 True)
                       (prog2 :: GeneticSearch Result (Maybe Result))


    anneal args = do
      putStrLn "simulated annealing search"
      case args of
        [] ->
          SA.runSearch (SA.Config bitCount 1 iterations1 0.05 200.0 10000.0 1 True)
                       (prog1 :: SimulatedAnnealingSearch Result (Maybe Result))
        ["SEQTH"] ->
          SA.runSearch (SA.Config bitCount 1 iterations1 0.05 200.0 10000.0 1 True)
                       (prog1 :: SimulatedAnnealingSearch Result (Maybe Result))  
        ["BOTH"]    ->
          SA.runSearch (SA.Config bitCount 2 iterations2 0.05 200.0 10000.0 1 True)
                       (prog2 :: SimulatedAnnealingSearch Result (Maybe Result))



-----------------------------------------------------------------
-- PROGS
-----------------------------------------------------------------

-- 2d search both params 
prog2 :: (MonadIO (m Result), SearchMonad Result m)
      => m Result (Maybe Result)
prog2 = do

  -- ctx <- liftIO $ initialize

  w_param <- getParam 0
  t_param <- getParam 1

  let seq_th = 2^(w_param `mod` 13)
      threads = 32 * (t_param + 1)


  liftIO $ putStrLn $ "Trying with threads = " ++ (show threads)
  liftIO $ putStrLn $ "And seq_th = " ++ (show seq_th)

  score <- liftIO $ scoreIt reductions2 4096 1024 64 threads seq_th

  liftIO $ putStrLn $ "Score = " ++ show score 
  return $ Just $ Result ([seq_th,threads],score)         

  
prog1 :: (MonadIO (m Result), SearchMonad Result m)
      => m Result (Maybe Result)
prog1 = do 
  w_param <- getParam 0

  let seq_th = 2^(w_param `mod` 13)
      threads = 128 -- 32 * (t_param + 1)

  liftIO $ putStrLn $ "Trying with threads = " ++ (show threads)
  liftIO $ putStrLn $ "And seq_th = " ++ (show seq_th)

  score <- liftIO $ scoreIt reductions2 4096 1024 64 threads seq_th

  liftIO $ putStrLn $ "Score = " ++ show score 
  return $ Just $ Result ([seq_th],score)         

 
scoreIt kernel chunk_size n_chunks blocks threads seq_th = do
  withCUDA $
    do
      kern <- capture (fromIntegral threads)
              ( ((kernel (fromIntegral seq_th) (+)) . splitUp (fromIntegral chunk_size)) :: Pull EWord32 EWord32 -> Push Grid EWord32 EWord32)

      (inputs :: V.Vector Word32) <- liftIO $ mkRandomVec (chunk_size * n_chunks)
      
      useVector inputs $ \input -> 
        withVector n_chunks $ \output ->
        do
          fill input 1
          syncAll
          t0   <- liftIO $ getCurrentTime
          output <== (blocks, kern) <> input
          syncAll 
          t1   <- liftIO $ getCurrentTime
          res <- peekCUDAVector output
          lift $ putStrLn $ show (take 10 res )

          return $ realToFrac $ diffUTCTime t1 t0 
