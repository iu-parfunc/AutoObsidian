
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
import Auto.BitClimbSearch    as BS
import Auto.ExhaustiveSearch  as ES
import Auto.GeneticSearch     as GS
import Auto.RandomSearch      as RS
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
imageSize, identity, count, maxNum, bitCount, popCount :: Int
--blocks    = 64
imageSize = 1024
identity  = 256
count     = 10
maxNum    = 960
bitCount  = 10
popCount  = 8

-- testing
main = do

  args <- getArgs

  -- hacked up argument handling
  -- TODO: Uniform handling of this across all apps would be nice.
  --       Some good argument handling!
  --       This is just a mess of copynpaste code
  res <- case args of
     ("RANDOM":_) -> random (tail args)
     ("BITCLIMB":_) -> bitclimb (tail args)
     ("EXHAUSTIVE":_) -> exhaustive (tail args)
     ("SGA":_) -> genetic (tail args)
     _ -> exhaustive []

  let filename = argsToFileName args
  let (b, Just a) = resultCSV res
  writeFile filename $ a


  let resultOverTime = unlines
        $ map (\(iter,Result p) -> show iter ++ ", " ++ show (snd p))
                                 (resultLogBestOverTime res)
  writeFile ("timeseries"++filename) resultOverTime




   where
     argsToFileName [] = "mandel_EXHAUSTIVE_THREADS.csv"
     argsToFileName [x] = "mandel_" ++ x ++ "_THREADS.csv"
     argsToFileName [x,y] = "mandel_" ++ x ++ "_" ++ y ++ ".csv"

     exhaustive args = do
       putStrLn "Exhaustive search"
       case args of
         [] ->
           execSearch (ES.Config [ [x*32| x <- [1..32]]])
                      (prog1 :: ExhaustiveSearch Result (Maybe Result))
         ["THREADS"] ->
           execSearch (ES.Config [ [x*32| x <- [1..32]]])
                      (prog1 :: ExhaustiveSearch Result (Maybe Result))
         ["BOTH"]    ->
           execSearch (ES.Config [ [x*32| x <- [1..32]]
                                 , [x*32| x <- [1..32]]])
                      (prog2 :: ExhaustiveSearch Result (Maybe Result))


     random args = do
       putStrLn "Random search"
       case args of
         [] ->
           execSearch (RS.Config [(0,1024)] 100)
                      (prog1 :: RandomSearch Result (Maybe Result))
         ["THREADS"] ->
           execSearch (RS.Config [(0,1024)] 100)
                      (prog1 :: RandomSearch Result (Maybe Result))
         ["BOTH"]    ->
           execSearch (RS.Config [(0,1024),(0,1024)] 100)
                      (prog2 :: RandomSearch Result (Maybe Result))


     bitclimb args = do
       putStrLn "Bit climb search"
       case args of
         [] ->
           execSearch (BS.Config bitCount 1 100 True)
                      (prog1 :: BitClimbSearch Result (Maybe Result))
         ["THREADS"] ->
           execSearch (BS.Config bitCount 1 100 True)
                      (prog1 :: BitClimbSearch Result (Maybe Result))

         ["BOTH"]    ->
           execSearch (BS.Config bitCount 2 100 True)
                      (prog2 :: BitClimbSearch Result (Maybe Result))

     genetic args = do
       putStrLn "Simple genetic algorithm"
       case args of
        [] ->
          execSearch (GS.Config bitCount 1 popCount 100 0.2 3 True)
                     (prog1 :: GeneticSearch Result (Maybe Result))
        ["THREADS"] ->
          execSearch (GS.Config bitCount 1 popCount 100 0.2 3 True)
                     (prog1 :: GeneticSearch Result (Maybe Result))
        ["BOTH"]    ->
          execSearch (GS.Config bitCount 2 popCount 100 0.2 3 True)
                     (prog2 :: GeneticSearch Result (Maybe Result))


-- 2d search Both params
prog2 :: (MonadIO (m Result), SearchMonad Result m)
       => m Result (Maybe Result)
prog2 = do

  -- This needs to be made part of the configuration of the search
  ctx <- liftIO $ initialise

  threads <- getParam 0
  blocks  <- getParam 1

  liftIO $ putStrLn $ "Trying with threads = " ++ (show threads)
  liftIO $ putStrLn $ "And blocks = " ++ (show blocks)


  if (threads <= 0)
    then do liftIO $ destroyCtx ctx
            return $ Nothing
    else body ctx threads blocks

  where
    body ctx threads blocks = do
      kern <- liftIO $ captureIO ("kernel" ++ show identity)
                                 (props ctx)
                                 (fromIntegral threads)
                                 (mandel (fromIntegral imageSize))

      -- Time the body of this instead...
      let runIt =
            liftIO $ withCUDA' ctx $ do
              withVector (fromIntegral (imageSize*imageSize)) $ \o -> do
                forM_ [0..count] $ \_ -> do
                  o <== (fromIntegral blocks,kern)
                  syncAll
                  copyOut o
      liftIO $ catch
        ( do
             -- This could be criterion instead, but it was complicated
             t0   <- getCurrentTime
             runIt >> return ()
             t1   <- getCurrentTime

             let timed = realToFrac $ diffUTCTime t1 t0
             putStrLn $ "Time for threads=" ++ (show threads) ++ "and blocks=" ++ show blocks ++ " was " ++ (show timed)
             destroyCtx ctx
             return $ Just $ Result ([threads,blocks],timed)

        )
        (\e -> do putStrLn (show (e :: SomeException))
                  destroyCtx ctx
                  return Nothing
        )



-- 1d search (only threads)
prog1 :: (MonadIO (m Result), SearchMonad Result m)
      => m Result (Maybe Result)
prog1 = do

  -- This needs to be made part of the configuration of the search
  ctx <- liftIO $ initialise

  let blocks = 64
  threads <- getParam 0

  liftIO $ putStrLn $ "Trying with threads = " ++ (show threads)

  if (threads <= 0)
    then do liftIO $ destroyCtx ctx
            return $ Nothing
    else body ctx threads blocks

  where
    body ctx threads blocks = do
      kern <- liftIO $ captureIO ("kernel" ++ show identity)
                                 (props ctx)
                                 (fromIntegral threads)
                                 (mandel (fromIntegral imageSize))

      -- Time the body of this instead...
      let runIt =
            liftIO $ withCUDA' ctx $ do
              withVector (fromIntegral (imageSize*imageSize)) $ \o -> do
                forM_ [0..count] $ \_ -> do
                  o <== (fromIntegral blocks,kern)
                  syncAll
                  copyOut o
      liftIO $ catch
        ( do
             -- This could be criterion instead, but it was complicated
             t0   <- getCurrentTime
             runIt >> return ()
             t1   <- getCurrentTime

             let timed = realToFrac $ diffUTCTime t1 t0
             putStrLn $ "Time for threads=" ++ (show threads) ++
                        " was " ++ (show timed)
             destroyCtx ctx
             return $ Just $ Result ([threads],timed)

        )
        (\e -> do putStrLn (show (e :: SomeException))
                  destroyCtx ctx
                  return Nothing
        )


