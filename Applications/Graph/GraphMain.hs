{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (replicate)
import Prelude as P

import Obsidian.Run.CUDA.Exec
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

-- shell
import System.Process
import System.IO

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
  
  putStrLn "Exhaustive search"
  res <- execSearch (ES.Config [[128, 256, 384, 512, 640, 768, 896, 1024, 1152]]) (prog :: ExhaustiveSearch Result (Maybe Result))
  putStrLn "Best param"
  putStrLn $ show $ peek $ resultLogBest res

  
                     
prog :: SearchMonad Result m => m Result (Maybe Result)
prog = do
  -- Get param settings 
  kernel_th <- getParam 0

  liftIO $ putStrLn $ "Trying with kernel_th = " ++ show kernel_th

  liftIO $ buildIt kernel_th 

  r <- liftIO $ runIt
  return $ Just $ Result ([kernel_th],r) 

buildIt :: Int -> IO () 
buildIt kernel_th = do
  (_,_,_,ph) <- createProcess $ shell cmd
  waitForProcess ph
  return () 

  where
    cmd = "(cd ./gpu_graph/iu_bfsdp; " ++
          "TUNE_PARAMS=-DKERNEL_TH="++ show kernel_th  ++
          " make -f Makefile)" 
  

runIt :: IO Double
runIt = do
  (_,Just sout,Just serr,ph) <- createProcess (shell cmd)
                                 {std_out = CreatePipe
                                 ,std_err = CreatePipe }

  -- should look at output for
  -- "cuda event timer: 0.017409 s, or 17.409281 ms" 
  output <- hGetContents sout
  let ls = lines output
  putStrLn $ show (length ls) ++ " LINES HARVESTERED"

  putStrLn $ show (ls !! 3) 
  
  waitForProcess ph
  return 5.0
  where
    -- with default params 
    cmd = "(cd ./gpu_graph/iu_bfsdp; ./main 20 16 0 0)" 
    -- num nodes (2^X)
    -- fanout per node
    -- I forgot
    -- What GPU to use 

