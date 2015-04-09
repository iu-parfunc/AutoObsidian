
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

-- import qualified Data.Vector.Storable as V
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

-- Autotuning framework 
import Auto.Score
import System.Random (mkStdGen, random,randomR, StdGen, newStdGen)
import GA (Entity(..), GAConfig(..), evolveVerbose)

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

  res <- runSearch (RNDConfig [(0,1024)] 100) (prog :: RandomSearch Result)

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


-- List of parameters and score
-- Abstract this further ? It allows only Int parameters
-- and Double results... 
type Result = Maybe ([Int],Double)


-- Trying out some method of abstracting the whole thing...

class (Monad m, MonadIO m) => SearchMonad m where
  type SearchConfig m 
  runSearch :: SearchConfig m -> m (Result) -> IO Result   
  getParam :: Int -> m Int

-- Config for Random search 
data RNDConfig = RNDConfig { paramRanges :: [(Int,Int)]
                           , numIters :: Int }

newtype RandomSearch a =
  RandomSearch (ReaderT RNDConfig (StateT (Result,StdGen)  IO)  a) 
 deriving ( Monad
          , MonadIO 
          , MonadState (Result, StdGen)
          , MonadReader RNDConfig
          , Functor
          , Applicative)

instance SearchMonad RandomSearch where
  type SearchConfig RandomSearch = RNDConfig
  getParam i = do
    cfg <- ask
    --- Here --- 
    (r,g) <- get   
    let range = (paramRanges cfg) !! i
        (a,g') = randomR range g
    put (r,g')
    --- To here -- can be replaced with a modify 
    return a 


  runSearch cfg (RandomSearch m) = do
    stdGen <- newStdGen -- splits some "global" generator

    -- number of runs is now configurable
    let m' = forM_ [1..(numIters cfg)] $ \experiment_num ->
          -- experiment_num could be used for something (info printing)
          do
            (r,g) <- get 
            res <- m
            case res of
              Nothing -> put(r,g) 
              Just (params,r') -> 
                case r of -- if old r is nothing, replace with new
                  Nothing -> put (res,g) 
                  --otherwise compare
                  Just (_,old_r) -> if (r' < old_r)
                             then do put (res,g) 
                                     return () 
                             else return ()

            -- TODO: Add a stack of 10 best so far
    
    (a,s) <- runStateT (runReaderT m' cfg) (Nothing,stdGen)
    return (fst s) 
