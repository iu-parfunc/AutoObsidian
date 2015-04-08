{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Fractals

import Prelude hiding (replicate, writeFile)
import Prelude as P

-- import Obsidian
import Obsidian.Run.CUDA.Exec

-- import qualified Data.Vector.Storable as V
import Control.Monad.State

-- Autotuning framework 
import Auto.Score
import System.Random (mkStdGen, random)
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

-- genetic algorithm
instance Entity BitString Double Context () IO where

  -- Generate a random starting number between 0 and maxNum.
  genRandom _ seed = return $ 
    let n = 1 + ((fst $ random $ mkStdGen seed) `mod` maxNum)
    in numToBitString n numBits

  -- Ideally we'd be working with a bit neighborhood here for the Int,
  -- rather than just having a few bits of arithmetic.
  crossover _ _ seed e1 e2 = return $ Just $
    let i = (fst $ random $ mkStdGen seed) `mod` numBits
    in crossBitString e1 e2 i

  -- Dumb mutation function. Should be smarter, see above.
  mutation _ _ seed e = return $ Just $
    let i = (fst $ random $ mkStdGen seed) `mod` numBits
    in mutateBitString e i

  -- Return a number denoting a score. Lower is better.
  score ctx e = do
    
    -- Should memoize this function with a mutable table!
    
    let threads = bitStringToNum e
    
    putStrLn $ "Trying with threads = " ++ (show threads)

    kern <- captureIO ("kernel" ++ show identity)
            (props ctx)
            (fromIntegral threads)
            (mandel (fromIntegral imageSize))

    let runIt = withCUDA' ctx $ do 
          allocaVector (fromIntegral (imageSize*imageSize)) $ \o -> do
            forM_ [0..count] $ \_ -> do
              o <== (fromIntegral imageSize,kern)
              syncAll
              copyOut o
    if threads == 0 then return Nothing else catch
      ( do
           -- This could be criterion instead, but it was complicated
           t0   <- getCurrentTime
           runIt >> return ()
           t1   <- getCurrentTime
           let timed = realToFrac $ diffUTCTime t1 t0
           putStrLn $ "Time for " ++ (show threads) ++ " was " ++ (show timed)
           return $ Just timed
      )
      (\e -> do putStrLn (show (e :: SomeException))
                return Nothing)

main = do
  let cfg = GAConfig 
            5 -- population size
            10 -- archive size (best entities to keep track of)
            10 -- maximum number of generations
            0.8 -- crossover rate (% of entities by crossover)
            0.2 -- mutation rate (% of entities by mutation)
            0.0 -- parameter for crossover (not used here)
            0.2 -- parameter for mutation (% of replaced letters)
            False -- whether or not to use checkpointing
            False -- don't rescore archive in each generation
  
  currTime <- getCurrentTime
  let timed = floor $ utctDayTime currTime :: Int
  let g = mkStdGen timed -- random generator
  ctx <- initialise 
  es <- evolveVerbose g cfg () ctx
  let e = snd $ P.head es :: BitString
      v = bitStringToNum e
  putStrLn $ "best entity: " ++ (show v)
  
  


-- -- Vary number of threads/block and image size  
-- main = do
--   putStrLn "Autotuning Mandelbrot fractal kernel"

--   ctx <- initialise

  
--   kern <- captureIO ("kernel" ++ show identity)
--           (props ctx)
--           threads
--           (mandel image_size)
  
 
--   let runIt = withCUDA' ctx $
--         allocaVector (fromIntegral (image_size*image_size)) $ \o -> 
--           do
--             -- threads here means blocks.
--             o <== (blocks,kern)
--             syncAll
--             copyOut o 

 
--   report <- catch
--     (
--       do
--         -- the one (1) is "experiment-number"
--         report <- withConfig (defaultConfig {verbosity = Verbose} ) $
--                   runAndAnalyseOne 1 ("ImageSize " ++ show image_size)
--                                      (whnfIO (runIt >> return ()))
--         return $ Just report
--     )
--     (\e -> do putStrLn (show (e :: SomeException))
--               return Nothing
--               )

  
--   case report of
--     Just report -> do
--       putStrLn $ show (reportName report)
--   --  putStrLn $ show (reportMeasured report) 
--       putStrLn $ show (reportAnalysis report)
--     Nothing -> putStrLn "run failed" 
    
--  -- defaultMainWith (defaultConfig {forceGC = True})
--  --      [bgroup ("ImageSize " ++ show image_size)
--  --       [ bench "Obsidian" $ whnfIO runIt ]
--  --      ]
