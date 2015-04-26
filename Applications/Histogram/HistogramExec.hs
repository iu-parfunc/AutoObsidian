
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Histogram

import Prelude hiding (replicate)
import Prelude as P

import Obsidian.Run.CUDA.Exec
import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word


testit :: IO ()
testit =
  withCUDA $
  do
    kern <- capture 256 (histogram 1 1024)

    useVector (V.fromList (P.replicate 1024 1)) $ \i ->
      withVector 10 $ \ (m :: CUDAVector Word32) ->
      do
        fill m 0
        exec $ (4,kern) <:> m <> i
        r <- peekCUDAVector m
        lift $ putStrLn $ show r


main :: IO ()
main = testit 
