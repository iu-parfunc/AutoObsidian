
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Histogram

import Prelude hiding (replicate)
import Prelude as P

import Obsidian.Run.CUDA.Exec
import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word
import Data.List 
import Data.Maybe 

-- Autotuning framework 
import Auto.Score
import Auto.ResultLog 
import Auto.SearchMonad
import Auto.RandomSearch as RS
import Auto.ExhaustiveSearch as ES
import Auto.BitClimbSearch as BS
import Auto.GeneticSearch as GS
import Auto.SimulatedAnnealingSearch as SA

-- timing
import Data.Time.Clock

-- exception
import Control.Exception

-- System 
import System.IO
import System.Environment

---------------------------------------------------------------------------

data Result = Result ([Int],Double)

instance Annealable Result where
  extract (Result (_,d)) = d

instance Eq Result where
   (Result (_,d1)) == (Result (_,d2)) = d1 == d2

instance Ord Result where
  compare (Result (_,d1)) (Result (_,d2)) = compare d1 d2

instance Show Result where
  show (Result (ls,r)) = show ls ++ " | " ++ show r

instance CSV Result where
  toCSVRow (Result (xs, d)) =
    unwords (intersperse "," (map show xs)) ++ "," ++ show d 



generations, iterations, popCount, bitCount, domainBitCount :: Int
bitCount = 10
domainBitCount = 5
popCount = 5
generations = 10
iterations = popCount * generations

main :: IO ()
main = do

  args <- getArgs
      
  -- Same vector for all runs, or pointless! 
  (inputs' :: V.Vector Word32) <- mkRandomVec (1024*1024)
  let inputs = V.map (`mod` 1024) $ inputs'
    
  res <- case args of
    ("RANDOM":_) -> random inputs (tail args)
    ("BITCLIMB":_) -> bitclimb inputs (tail args)
    ("EXHAUSTIVE":_) -> exhaustive inputs (tail args)
    ("SGA":_) -> genetic inputs (tail args)
    ("SA":_) -> anneal inputs (tail args)
    ("DOMAIN_BITCLIMB":_) -> domainBitclimb inputs (tail args)
    ("DOMAIN_SGA":_) -> domainGenetic inputs (tail args)
    ("DOMAIN_SA":_) -> domainAnneal inputs (tail args)
    _ -> exhaustive inputs []
    
  let filename = argsToFileName args
  let (b, Just a) = resultCSV res
  writeFile filename $ a 

  let resultOverTime = unlines 
        $ map (\(iter,Result p) -> show iter ++ ", " ++ show (snd p))
                                 (resultLogBestOverTime res)
  writeFile ("timeseries"++filename) resultOverTime 


  
  where
    argsToFileName [] = "histo_EXHAUSTIVE_THREADS.csv"
    argsToFileName [x] = "histo_" ++ x ++ "_TREADS.csv"
    argsToFileName [x,y] = "histo_" ++ x ++ "_" ++ y ++ ".csv" 

    exhaustive inputs args = do
      putStrLn "Exhaustive search"
      case args of
        [] -> 
          ES.runSearch (ES.Config [[x*32 | x <- [1..32]]])
             (prog inputs :: ExhaustiveSearch Result (Maybe Result))
        ["THREADS"] -> 
          ES.runSearch (ES.Config [[x*32 | x <- [1..32]]])
             (prog inputs :: ExhaustiveSearch Result (Maybe Result))
        ["BOTH"] ->
          ES.runSearch (ES.Config [ [x*32 | x <- [1..32]]
                                , [x*32 | x <- [1..32]]])
             (prog2 inputs :: ExhaustiveSearch Result (Maybe Result))

    random inputs args = do
      putStrLn "Random search"
      case args of
         [] ->
           RS.runSearch (RS.Config [(0,1024)] iterations)
                      (prog inputs :: RandomSearch Result (Maybe Result))
         ["THREADS"] ->
           RS.runSearch (RS.Config [(0,1024)] iterations)
                      (prog inputs :: RandomSearch Result (Maybe Result))
         ["BOTH"]    ->
           RS.runSearch (RS.Config [(0,1024),(0,1024)] iterations)
                      (prog2 inputs :: RandomSearch Result (Maybe Result))


    bitclimb inputs args = do
      putStrLn "Bit climb search"
      case args of
         [] ->
           BS.runSearch (BS.Config bitCount 1 iterations 1 True)
                      (prog inputs :: BitClimbSearch Result (Maybe Result))
         ["THREADS"] ->
           BS.runSearch (BS.Config bitCount 1 iterations 1 True)
                      (prog inputs :: BitClimbSearch Result (Maybe Result))

         ["BOTH"]    ->
           BS.runSearch (BS.Config bitCount 2 iterations 1 True)
                      (prog2 inputs :: BitClimbSearch Result (Maybe Result))

    anneal inputs args = do
      putStrLn "simulated annealing search"
      case args of
         [] ->
           SA.runSearch (SA.Config bitCount 1 iterations 0.05 200.0 10.0 1 True)
                      (prog inputs :: SimulatedAnnealingSearch Result (Maybe Result))
         ["THREADS"] ->
           SA.runSearch (SA.Config bitCount 1 iterations 0.05 200.0 10.0 1 True)
                      (prog inputs :: SimulatedAnnealingSearch Result (Maybe Result))
         ["BOTH"]    ->
           SA.runSearch (SA.Config bitCount 2 iterations 0.05 200.0 10.0 1 True)
                      (prog2 inputs :: SimulatedAnnealingSearch Result (Maybe Result))

    genetic inputs args = do
      putStrLn "Simple genetic algorithm"
      case args of
        [] ->
          GS.runSearch (GS.Config bitCount 1 popCount generations 0.2 3 1 True)
                       (prog inputs :: GeneticSearch Result (Maybe Result))
        ["THREADS"] ->
          GS.runSearch (GS.Config bitCount 1 popCount generations 0.2 3 1 True)
                       (prog inputs :: GeneticSearch Result (Maybe Result))
        ["BOTH"] ->
          GS.runSearch (GS.Config bitCount 2 popCount generations 0.2 3 1 True)
                       (prog2 inputs :: GeneticSearch Result (Maybe Result))


    domainBitclimb inputs args = do
      putStrLn "Bit climb search"
      case args of
         [] ->
           BS.runSearch (BS.Config domainBitCount 1 (iterations `div` 2) 32 True)
                      (prog inputs :: BitClimbSearch Result (Maybe Result))
         ["THREADS"] ->
           BS.runSearch (BS.Config domainBitCount 1 (iterations `div` 2) 32 True)
                      (prog inputs :: BitClimbSearch Result (Maybe Result))

         ["BOTH"]    ->
           BS.runSearch (BS.Config domainBitCount 2 (iterations `div` 2) 32 True)
                      (prog2 inputs :: BitClimbSearch Result (Maybe Result))

    domainGenetic inputs args = do
      putStrLn "Simple genetic algorithm"
      case args of
        [] ->
          GS.runSearch (GS.Config domainBitCount 1 popCount (generations `div` 2) 0.2 3 32 True)
                       (prog inputs :: GeneticSearch Result (Maybe Result))
        ["THREADS"] ->
          GS.runSearch (GS.Config domainBitCount 1 popCount (generations `div` 2) 0.2 3 32 True)
                       (prog inputs :: GeneticSearch Result (Maybe Result))
        ["BOTH"] ->
          GS.runSearch (GS.Config domainBitCount 2 popCount (generations `div` 2) 0.2 3 32 True)
                       (prog2 inputs :: GeneticSearch Result (Maybe Result))

    domainAnneal inputs args = do
      putStrLn "simulated annealing search"
      case args of
         [] ->
           SA.runSearch (SA.Config domainBitCount 1 iterations 0.05 50.0 10.0 32 True)
                      (prog inputs :: SimulatedAnnealingSearch Result (Maybe Result))
         ["THREADS"] ->
           SA.runSearch (SA.Config domainBitCount 1 iterations 0.05 50.0 10.0 32 True)
                      (prog inputs :: SimulatedAnnealingSearch Result (Maybe Result))
         ["BOTH"]    ->
           SA.runSearch (SA.Config domainBitCount 2 iterations 0.05 50.0 10.0 32 True)
                      (prog2 inputs :: SimulatedAnnealingSearch Result (Maybe Result))


  
                     
prog2 :: (MonadIO (m Result), SearchMonad Result m) 
     => V.Vector Word32 -> m Result (Maybe Result)
prog2 inputs = do
  ctx <- liftIO initialise


  -- Get param settings 
  threads <- getParam 0
  blocks  <- getParam 1

  liftIO $ putStrLn $ "Trying with threads = " ++ show threads ++ " and blocks = " ++ show blocks 

  if (threads <= 0 || blocks <= 0)
     then do liftIO $ destroyCtx ctx
             return Nothing
    else body ctx threads blocks

  where
    body :: (MonadIO (m Result), SearchMonad Result m) 
         => Context -> Int -> Int -> m Result (Maybe Result) 
    body ctx threads blocks = do
      kern <- liftIO $ captureIO "histogram" (props ctx) 
                                 (fromIntegral threads)
                                 (histogram 1024 1024)

      
      let runIt =
            liftIO $ withCUDA' ctx $ do
              useVector inputs $ \i -> 
                withVector (1024) $ \ o -> 
                  forM_ [0..999] $ \_ -> do
                   fill o 0 
                   exec $ (fromIntegral blocks, kern) <:> o <> i 
                   syncAll 
                   copyOut o 

      liftIO $ catch
        ( do
             -- This could be criterion instead, but it was complicated
             t0   <- getCurrentTime
             runIt >> return ()
             t1   <- getCurrentTime

             let timed = realToFrac $ diffUTCTime t1 t0
             putStrLn $ "Time for " ++ (show threads) ++ " / " ++ show blocks ++ " was " ++ (show timed)
             destroyCtx ctx
             return $ Just $ Result ([threads,blocks],timed)

        )
        (\e -> do putStrLn (show (e :: SomeException))
                  destroyCtx ctx 
                  return Nothing
        )





prog :: (MonadIO (m Result), SearchMonad Result m) 
     => V.Vector Word32 -> m Result (Maybe Result)
prog inputs = do
  ctx <- liftIO initialise


  -- Get param settings 
  threads <- getParam 0
  let blocks = 64 

  liftIO $ putStrLn $ "Trying with threads = " ++ show threads ++ " and blocks = " ++ show blocks 

  if (threads <= 0 || blocks <= 0)
     then do liftIO $ destroyCtx ctx
             return Nothing
    else body ctx threads blocks

  where
    body :: (MonadIO (m Result), SearchMonad Result m) 
         => Context -> Int -> Int -> m Result (Maybe Result) 
    body ctx threads blocks = do
      kern <- liftIO $ captureIO "histogram" (props ctx) 
                                 (fromIntegral threads)
                                 (histogram 1024 1024)

      
      let runIt =
            liftIO $ withCUDA' ctx $ do
              useVector inputs $ \i -> 
                withVector (1024) $ \ o -> 
                  forM_ [0..999] $ \_ -> do
                   fill o 0 
                   exec $ (fromIntegral blocks, kern) <:> o <> i 
                   syncAll 
                   copyOut o 

      liftIO $ catch
        ( do
             -- This could be criterion instead, but it was complicated
             t0   <- getCurrentTime
             runIt >> return ()
             t1   <- getCurrentTime

             let timed = realToFrac $ diffUTCTime t1 t0
             putStrLn $ "Time for " ++ (show threads) ++ " / " ++ show blocks ++ " was " ++ (show timed)
             destroyCtx ctx
             return $ Just $ Result ([threads],timed)

        )
        (\e -> do putStrLn (show (e :: SomeException))
                  destroyCtx ctx 
                  return Nothing
        )



