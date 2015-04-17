
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
  show (Result (_,r)) = show r

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

  putStrLn "Bit climb search"
  res <- runSearch (BS.Config bitCount 1 100) (prog :: BitClimbSearch Result (Maybe Result))
  putStrLn "Best param"
  putStrLn $ show res

  putStrLn "Random search"
  res <- runSearch (RS.Config [(0,1024)] 100) (prog :: RandomSearch Result (Maybe Result))
  putStrLn "Best param"
  putStrLn $ show res 

  putStrLn "Exhaustive search"
  res <- runSearch (ES.Config [[32,64,128,256]]) (prog :: ExhaustiveSearch Result (Maybe Result))
  putStrLn "Best param"
  putStrLn $ show res

prog :: SearchMonad Result m => m Result (Maybe Result)
prog = do
  -- This needs to be made part of the configuration of the search 
  ctx <- liftIO $ initialise

  threads <- getParam 0 
  
  liftIO $ putStrLn $ "Trying with threads = " ++ (show threads)

  kern <- liftIO $ captureIO ("kernel" ++ show identity)
          (props ctx)
          (fromIntegral threads)
          (mandel (fromIntegral imageSize))

  -- Time the body of this instead... 
  let runIt = liftIO $ withCUDA' ctx $ do 
        withVector (fromIntegral (imageSize*imageSize)) $ \o -> do
          forM_ [0..count] $ \_ -> do
            o <== (fromIntegral imageSize,kern)
            syncAll
            copyOut o
  if threads == 0
    then return Nothing
    else liftIO $ catch
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


