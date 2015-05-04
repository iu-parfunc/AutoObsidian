
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

-- System 
import System.IO
import System.Environment

---------------------------------------------------------------------------

data Result = Result ([Int],Double)

instance Eq Result where
   (Result (_,d1)) == (Result (_,d2)) = d1 == d2

instance Ord Result where
  compare (Result (_,d1)) (Result (_,d2)) = compare d1 d2

instance Show Result where
  show (Result (ls,r)) = show ls ++ " | " ++ show r

instance CSV Result where
  toCSVRow (Result (xs, d)) =
    unwords (intersperse "," (map show xs)) ++ "," ++ show d 



bitCount :: Int
bitCount = 10 

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
    _ -> exhaustive inputs []
    
  let filename = argsToFileName args
  let (b, Just a) = resultCSV res
  writeFile filename $ a 

  
  where
    argsToFileName [] = "histo_EXHAUSTIVE_THREADS.csv"
    argsToFileName [x] = "histo_" ++ x ++ "_TREADS.csv"
    argsToFileName [x,y] = "histo_" ++ x ++ "_" ++ y ++ ".csv" 

    exhaustive inputs args = do
      putStrLn "Exhaustive search"
      case args of
        [] -> 
          execSearch (ES.Config [[x*32 | x <- [1..32]]])
             (prog inputs :: ExhaustiveSearch Result (Maybe Result))
        ["THREADS"] -> 
          execSearch (ES.Config [[x*32 | x <- [1..32]]])
             (prog inputs :: ExhaustiveSearch Result (Maybe Result))
        ["BOTH"] ->
          execSearch (ES.Config [ [x*32 | x <- [1..32]]
                                , [x*32 | x <- [1..32]]])
             (prog2 inputs :: ExhaustiveSearch Result (Maybe Result))

    random inputs args = do
      putStrLn "Random search"
      case args of
         [] ->
           execSearch (RS.Config [(0,1024)] 100)
                      (prog inputs :: RandomSearch Result (Maybe Result))
         ["THREADS"] ->
           execSearch (RS.Config [(0,1024)] 100)
                      (prog inputs :: RandomSearch Result (Maybe Result))
         ["BOTH"]    ->
           execSearch (RS.Config [(0,1024),(0,1024)] 100)
                      (prog2 inputs :: RandomSearch Result (Maybe Result))


    bitclimb inputs args = do
      putStrLn "Bit climb search"
      case args of
         [] ->
           execSearch (BS.Config bitCount 1 100 True)
                      (prog inputs :: BitClimbSearch Result (Maybe Result))
         ["THREADS"] ->
           execSearch (BS.Config bitCount 1 100 True)
                      (prog inputs :: BitClimbSearch Result (Maybe Result))

         ["BOTH"]    ->
           execSearch (BS.Config bitCount 2 100 True)
                      (prog2 inputs :: BitClimbSearch Result (Maybe Result))



  
                     
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



