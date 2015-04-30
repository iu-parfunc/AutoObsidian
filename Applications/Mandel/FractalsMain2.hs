
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleContexts #-} 

module Main where

import Fractals

import Prelude hiding (replicate, writeFile)
import Prelude as P

-- import Obsidian
import Obsidian.Run.CUDA.Exec

-- Autotuning framework 
import Auto.Score
import Auto.ResultLog 
import Auto.SearchMonad
import Auto.RandomSearch as RS
import Auto.ExhaustiveSearch as ES
import Auto.BitClimbSearch as BS

-- -- timing
import Data.Time.Clock

--exception
import Control.Exception



data Result = Result ([Int],Double)

instance Eq Result where
  (Result (_,d1)) == (Result (_,d2)) = d1 == d2

instance Ord Result where
  compare (Result (_,d1)) (Result (_,d2)) = compare d1 d2

instance Show Result where
  show (Result (p,r)) = show p ++ " | " ++ show r

-- parameters
-- threads   = 256
blocks, imageSize, identity, count, maxNum, bitCount :: Int
blocks    = 64
imageSize = 1024
identity  = 256
count     = 10
maxNum    = 960
bitCount  = 10

-- testing 
main = do

  -- putStrLn "Bit climb search"
  -- res <- execSearch (BS.Config bitCount 1 100 True) (prog :: BitClimbSearch Result (Maybe Result))
  -- putStrLn "Best param"
  -- putStrLn $ show $ peek $ resultLogBest res

  putStrLn "Random search"
  res <- execSearch (RS.Config [(0,1024)] 100) (prog :: RandomSearch Result (Maybe Result))
  putStrLn "Best param"
  putStrLn $ show $ peek $ resultLogBest res 

  putStrLn "Exhaustive search"
  res <- execSearch (ES.Config [[x*32| x <- [1..32]]]) (prog :: ExhaustiveSearch Result (Maybe Result))
  putStrLn "Best param"
  putStrLn $ show $ peek $ resultLogBest res

prog :: (MonadIO (m Result), SearchMonad Result m) 
     => m Result (Maybe Result)
prog = do
  
  -- This needs to be made part of the configuration of the search 
  ctx <- liftIO $ initialise
  
  threads <- getParam 0
  
  liftIO $ putStrLn $ "Trying with threads = " ++ (show threads)


  if (threads <= 0)
    then do liftIO $ destroyCtx ctx
            return $ Nothing
    else body ctx threads 

  where 
    body ctx threads = do 
      kern <- liftIO $ captureIO ("kernel" ++ show identity)
                                 (props ctx)
                                 (fromIntegral threads)
                                 (mandel (fromIntegral imageSize))

      -- Time the body of this instead... 
      let runIt =
            liftIO $ withCUDA' ctx $ do 
              withVector (fromIntegral (imageSize*imageSize)) $ \o -> do
                forM_ [0..count] $ \_ -> do
                  o <== (fromIntegral imageSize,kern)
                  syncAll
                  copyOut o
      liftIO $ catch
        ( do
             -- This could be criterion instead, but it was complicated
             t0   <- getCurrentTime
             runIt >> return ()
             t1   <- getCurrentTime

             let timed = realToFrac $ diffUTCTime t1 t0
             putStrLn $ "Time for " ++ (show threads) ++ " was " ++ (show timed)
             destroyCtx ctx
             return $ Just $ Result ([threads],timed)

        )
        (\e -> do putStrLn (show (e :: SomeException))
                  destroyCtx ctx 
                  return Nothing
        )


