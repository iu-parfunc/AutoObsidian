
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-} 

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

-- -- timing
import Data.Time.Clock

-- import Criterion.Main
-- import Criterion.Types 
-- import Criterion.Monad
-- import Criterion.Internal

--exception
import Control.Exception

-- parameters
-- threads   = 256
blocks, imageSize, identity, count, maxNum, numBits :: Int
blocks    = 64
imageSize = 1024
identity  = 256
count     = 10
maxNum    = 960
numBits   = 10

-- testing 
main = do

  --res <- runSearch (RS.Config [(0,1024)] 100) (prog :: RandomSearch Result)

  res <- runSearch (ES.Config [[32,64,128,256]]) (prog :: ExhaustiveSearch Result)
  
  putStrLn "Best param"
  putStrLn $ show res 


prog :: SearchMonad m => m (Result)
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
              return $ Just ([threads],timed)

         )
         (\e -> do putStrLn (show (e :: SomeException))
                   destroyCtx ctx 
                   return Nothing
         )


