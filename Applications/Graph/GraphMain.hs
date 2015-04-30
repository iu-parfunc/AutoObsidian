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
import System.Process
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



main :: IO ()
main = do

  args <- getArgs

  res <- case args of
    [] -> exhaustive
    ["RANDOM"] -> random
    ["BITCLIMB"] -> bitclimb

  putStrLn "Best param"
  putStrLn $ show $ peek $ resultLogBest res
    
  where
    exhaustive = do
      putStrLn "Exhaustive search"
      execSearch (ES.Config [[128, 256, 384, 512, 640, 768, 896, 1024, 1152]])
                 (prog :: ExhaustiveSearch Result (Maybe Result))
      
    random = do
      putStrLn "Random search"
      execSearch (RS.Config [(1,1024)] 1000)
                 (prog :: RandomSearch Result (Maybe Result))
      
    bitclimb = do 
      putStrLn "Bit climb search"
      execSearch (BS.Config 10 1 100 True)
                 (prog :: BitClimbSearch Result (Maybe Result)) 

             
  
                     
prog :: SearchMonad Result m => m Result (Maybe Result)
prog = do
  -- Get param settings 
  kernel_th <- getParam 0

  liftIO $ putStrLn $ "Trying with kernel_th = " ++ show kernel_th

  liftIO $ buildIt kernel_th 

  r <- liftIO $ runIt
  liftIO $ putStrLn $ "Time for KERNEL_TH=" ++ (show kernel_th) ++ " was " ++ (show r)
  return $ Just $ Result ([kernel_th],r) 

buildIt :: Int -> IO () 
buildIt kernel_th = do
  putStrLn "Compiling.." 
  (_,_,_,ph) <- createProcess (shell cmd) { std_out = CreatePipe
                                          , std_err = CreatePipe }
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
  let prefix = "cuda event timer:" 

  
  output <- hGetContents sout
  let ls = lines output
  -- putStrLn $ show (length ls) ++ " LINES HARVESTERED"



  
  waitForProcess ph

  -- putStrLn $ show (ls !! 4)
  if ( isPrefixOf prefix (ls !! 4) )
    then let [(d,_)] = reads (drop (length prefix) (ls !! 4)) :: [(Double,String)]
         in return d
    else error "ERROR" 
  

  where
    -- with default params 
    cmd = "(cd ./gpu_graph/iu_bfsdp; ./main 20 16 0 0)" 
    -- num nodes (2^X)
    -- fanout per node
    -- I forgot
    -- What GPU to use 

