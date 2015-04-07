{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Fractals

import Prelude hiding (replicate, writeFile)
import Prelude as P

import Obsidian
import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int

import System.Environment
import System.Exit

-- Autotuning framework 
import Auto.Score

-- timing
import Criterion.Main
import Criterion.Types 
import Criterion.Monad
import Criterion.Internal

--exception
import Control.Exception

threads = 12256
blocks  = 64
image_size = 1024
identity = 256

-- Vary number of threads/block and image size  
main = do
  putStrLn "Autotuning Mandelbrot fractal kernel"

  ctx <- initialise

  
  kern <- captureIO ("kernel" ++ show identity)
          (props ctx)
          threads
          (mandel image_size)
  
 
  let runIt = withCUDA' ctx $
        allocaVector (fromIntegral (image_size*image_size)) $ \o -> 
          do
            -- threads here means blocks.
            o <== (blocks,kern)
            syncAll
            copyOut o 

  -- the one (1) is "experiment-number"
  report <- catch
    (
      do 
        report <- withConfig (defaultConfig {verbosity = Verbose} ) $
                  runAndAnalyseOne 1 ("ImageSize " ++ show image_size)
                                     (whnfIO (runIt >> return ()))
        return $ Just report
    )
    (\e -> do putStrLn (show (e :: SomeException))
              return Nothing
              )

  
  case report of
    Just report -> do
      putStrLn $ show (reportName report)
  --  putStrLn $ show (reportMeasured report) 
      putStrLn $ show (reportAnalysis report)
    Nothing -> putStrLn "run failed" 
    
 -- defaultMainWith (defaultConfig {forceGC = True})
 --      [bgroup ("ImageSize " ++ show image_size)
 --       [ bench "Obsidian" $ whnfIO runIt ]
 --      ]
