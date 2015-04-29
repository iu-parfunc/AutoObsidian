
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

-- Autotuning framework 
import Auto.Score
import Auto.ResultLog 
import Auto.SearchMonad
import Auto.RandomSearch as RS
import Auto.ExhaustiveSearch as ES
import Auto.BitClimbSearch as BS

-- timing
import Data.Time.Clock

-- exception
import Control.Exception

---------------------------------------------------------------------------

data Result = Result ([Int],Double)

instance Eq Result where
   (Result (_,d1)) == (Result (_,d2)) = d1 == d2

instance Ord Result where
  compare (Result (_,d1)) (Result (_,d2)) = compare d1 d2

instance Show Result where
  show (Result (ls,r)) = show ls ++ " | " ++ show r



main :: IO ()
main = do

  -- Same vector for all runs, or pointless! 
  (inputs' :: V.Vector Word32) <- mkRandomVec (1024*1024)
  let inputs = V.map (`mod` 1024) $ inputs'
    
  
  putStrLn "Exhaustive search"
  res <- execSearch (ES.Config [[32,64,128,256],[16,32,48,64]]) (prog inputs :: ExhaustiveSearch Result (Maybe Result))
  putStrLn "Best param"
  putStrLn $ show $ peek $ resultLogBest res

  
                     
prog :: SearchMonad Result m => V.Vector Word32 -> m Result (Maybe Result)
prog inputs = do
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
    body :: SearchMonad Result m => Context -> Int -> Int -> m Result (Maybe Result) 
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
      


-- testit :: IO ()
-- testit =
--   withCUDA $
--   do
--     kern <- capture 256 (histogram 1 1024)

--     useVector (V.fromList (P.replicate 1024 1)) $ \i ->
--       withVector 10 $ \ (m :: CUDAVector Word32) ->
--       do
--         fill m 0
--         exec $ (4,kern) <:> m <> i
--         r <- peekCUDAVector m
--         lift $ putStrLn $ show r


-- main :: IO ()
-- main = testit 
