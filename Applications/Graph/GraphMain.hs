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
import Data.Maybe 

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
{- Settings to tune
   CONFIG_SEQUENTIALIZE_SMALL_VERTICES = 1
   CONFIG_SMALL_VERTEX_THRESHOLD = 1 - 1000 ?

   KERNEL_TH = 1 .. 10000 ? 

-} 


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

main :: IO ()
main = do

  args <- getArgs

  res <- case args of
    ("RANDOM":_) -> random (tail args) 
    ("BITCLIMB":_) -> bitclimb (tail args) 
    ("EXHAUSTIVE":_) -> exhaustive (tail args) 
    _ -> exhaustive [] 

  putStrLn "Best param"
  putStrLn $ show $ peek $ resultLogBest res

  let filename = argsToFileName args
  let (b, Just a) = resultCSV res
  writeFile filename $ a


  let resultOverTime =
       zip [[x]|x <- [(1::Int)..]]
           (reverse $ (map (\(Result p) -> snd p)
                $ flifoData $ fromJust $ resultLogAll res))
  writeFile ("timeseries"++filename)
    $ unlines
    $ map (toCSVRow . Result) resultOverTime 




  where
    argsToFileName [] = "graph_EXHAUSTIVE_1.csv"
    argsToFileName [x] = "graph_" ++ x ++ "_1.csv"
    argsToFileName [x,y] = "graph_" ++ x ++ "_" ++ y ++ ".csv" 
    
    exhaustive args = do
      putStrLn "Exhaustive search"
      case args of
        [] -> 
          execSearch (ES.Config [[x*32 | x <- [1..64]]])
                     (prog :: ExhaustiveSearch Result (Maybe Result))
        ["1"] -> 
          execSearch (ES.Config [[x*32 | x <- [1..64]]])
                     (prog :: ExhaustiveSearch Result (Maybe Result))
        ["2"] ->
          execSearch (ES.Config [ [x*32 | x <- [1..64]]
                                , [x | x <- [1..64]]])
             (prog2Param :: ExhaustiveSearch Result (Maybe Result))
  
    random args = do
      putStrLn "Random search"
      case args of
        [] -> 
          execSearch (RS.Config [(1,1024)] 1000)
                     (prog :: RandomSearch Result (Maybe Result))
        ["1"] -> 
          execSearch (RS.Config [(1,1024)] 1000)
                     (prog :: RandomSearch Result (Maybe Result))
        ["2"] ->
          execSearch (RS.Config [(1,2048),(1,64)] 1000)
                     (prog2Param :: RandomSearch Result (Maybe Result))
          
    bitclimb args = do 
      putStrLn "Bit climb search"
      case args of
        [] -> 
          execSearch (BS.Config 10 1 100 True)
                     (prog :: BitClimbSearch Result (Maybe Result)) 
        ["1"] ->
          execSearch (BS.Config 10 1 100 True)
                     (prog :: BitClimbSearch Result (Maybe Result))
        ["2"] ->
          execSearch (BS.Config 10 2 100 True)
                     (prog2Param :: BitClimbSearch Result (Maybe Result))
             
  
                     
prog :: (MonadIO (m Result), SearchMonad Result m)
     => m Result (Maybe Result)
prog = do
  -- Get param settings 
  kernel_th <- getParam 0

  liftIO $ putStrLn $ "Trying with kernel_th = " ++ show kernel_th

  liftIO $ buildIt False 0 kernel_th 

  r <- liftIO $ runIt
  liftIO $ putStrLn $ "Time for KERNEL_TH=" ++ (show kernel_th) ++ " was " ++ (show r)
  return $ Just $ Result ([kernel_th],r) 

----------------------------------------------------------------------
--
----------------------------------------------------------------------
prog2Param :: (MonadIO (m Result), SearchMonad Result m)
     => m Result (Maybe Result)
prog2Param = do
  -- Get param settings 

  small_th <- getParam 1

  kernel_th <- getParam 0
  
  
  liftIO $ putStrLn $ "Trying with kernel_th = " ++ show kernel_th ++ "\n" ++ 
                      "and small_th = " ++ show small_th 

  liftIO $ buildIt True small_th kernel_th 

  r <- liftIO $ runIt
  liftIO $ putStrLn $ "Time for:\nKERNEL_TH=" ++
                      show kernel_th ++ "\n" ++
                      "CONFIG_SMALL_VERTEX_THRESHOLD=" ++
                      show small_th  ++ "\n was " ++ (show r)
  return $ Just $ Result ([kernel_th],r) 


----------------------------------------------------------------------
--
----------------------------------------------------------------------
buildIt :: Bool -> Int -> Int -> IO () 
buildIt sequentialize_small small_th kernel_th = do
  putStrLn "Compiling.."
  -- putStrLn cmd 
  
  (_,_,_,ph) <- createProcess (shell cmd) { std_out = CreatePipe
                                          , std_err = CreatePipe }
  waitForProcess ph
  return () 

  
  where
    small = if sequentialize_small
            then "-DCONFIG_SEQUENTIALIZE_SMALL_VERTICES=1"
            else ""
    small_th_str = if sequentialize_small
                   then "-DCONFIG_SMALL_VERTEX_THRESHOLD=" ++ show small_th
                   else "" 
    cmd = "(cd ./gpu_graph/iu_bfsdp; " ++
          "TUNE_PARAMS='-DKERNEL_TH="++ show kernel_th  ++
          " " ++ small ++ " " ++ small_th_str ++ "'" ++
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

