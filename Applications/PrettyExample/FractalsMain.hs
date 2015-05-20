
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Main where

import Fractals

import Prelude                hiding (replicate, writeFile)
import Prelude                as P

import Data.List

-- import Obsidian
import Obsidian.Run.CUDA.Exec

-- Autotuning framework
import Auto.BitClimbSearch           as BS
import Auto.ExhaustiveSearch         as ES
import Auto.GeneticSearch            as GS
import Auto.RandomSearch             as RS
import Auto.SimulatedAnnealingSearch as SA
import Auto.ResultLog
import Auto.SearchMonad

-- -- timing
import Data.Time.Clock

--exception
import Control.Exception

-- System
import System.Environment
import System.IO

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

-- parameters
-- threads   = 256
--blocks,
generations, iterations, imageSize, identity, count, maxNum, bitCount, domainBitCount, popCount :: Int
--blocks    = 64
imageSize = 1024
identity  = 256
count     = 10
maxNum    = 960
bitCount  = 10
popCount  = 5
domainBitCount = 5
generations = 10
iterations = popCount * generations


-- Iterations for heuristic searches: 50
-- (pop 5 times 10 generations)

-- testing
main = do

  resLog <- ES.runSearch (ES.Config [ [x*32| x <- [1..32]]
                                    , [x*32| x <- [1..32]]])
                         scoreIt

  let best = getBestResult resLog 

  case best of
    Nothing -> putStrLn "No results"
    Just r  -> putStrLn $ show r 
  putStrLn "Done!" 
            

scoreIt :: (MonadIO m, SearchMonad m)
       => m (Maybe Result)
scoreIt = do

  threads <- getParam 0
  blocks  <- getParam 1

  liftIO $ catch (
    do time <- timeIt threads blocks
       return $ Just $ Result ([threads,blocks],time)
    )
    (\e -> do putStrLn (show (e :: SomeException))
              return Nothing
    )
    
timeIt :: Int -> Int -> IO Double 
timeIt threads blocks = 
  withCUDA $ do 
    kern <- capture (fromIntegral threads)
                    (mandel (fromIntegral imageSize))

        -- Time the body of this instead...
    let runIt = do 
          withVector (fromIntegral (imageSize*imageSize)) $ \o -> do
            forM_ [0..count] $ \_ -> do
              o <== (fromIntegral blocks,kern)
              syncAll
              copyOut o
       

        -- This could be criterion instead, but it was complicated
    t0   <- liftIO $ getCurrentTime
    runIt >> return ()
    t1   <- liftIO $ getCurrentTime

    let timed = realToFrac $ diffUTCTime t1 t0
    liftIO $ putStrLn $ "Time for threads=" ++ (show threads) ++ "and blocks=" ++ show blocks ++ " was " ++ (show timed)
    return timed


