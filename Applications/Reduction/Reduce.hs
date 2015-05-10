{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where


import Obsidian
import Obsidian.CodeGen.CUDA
import Obsidian.Run.CUDA.Exec hiding (exec) 

import Data.Word
import Control.Monad.State 

import Prelude hiding (map,zipWith,sum,replicate,take,drop,iterate)
import qualified Data.Vector.Storable as V


-----------------------------------------------------------------
-- Reduction
-----------------------------------------------------------------
reduceKernel :: (Compute t, Data a) 
                => (a -> a -> a)
                -> Pull Word32 a 
                -> Program t (SPush t a)
reduceKernel f arr 
  | len arr == 1 = return $ push arr
  | otherwise =
    do let (a1,a2) = halve arr
       arr' <- compute $ zipWith f a1 a2
       reduceKernel f arr'

reductions :: Data a
           => (a -> a -> a)
           -> Pull EWord32 (SPull a)
           -> Push Grid EWord32 a
reductions f arr = asGridMap body arr
  where
    body arr = execBlock $ reduceKernel f arr

block_reduce :: Data a =>  (a -> a -> a) -> Pull Word32 a -> Push Block Word32 a 
block_reduce f = execBlock . reduceKernel f 

warp_reduce :: Data a =>  (a -> a -> a) -> Pull Word32 a -> Push Warp Word32 a 
warp_reduce f = execWarp . reduceKernel f 

block_reduce' :: Data a =>  (a -> a -> a) -> Pull Word32 a -> Program Block (Push Block Word32 a)
block_reduce'= reduceKernel

warp_reduce' :: Data a =>  (a -> a -> a) -> Pull Word32 a -> Program Warp (Push Warp Word32 a)
warp_reduce' = reduceKernel 


hybrid_reduce :: Data a
              => Word32 
              -> (a -> a -> a)
              -> Pull Word32 a -> Push Block Word32 a
hybrid_reduce warp_th f arr = execBlock $ b_body arr
  where
    b_body arr = do
      arr' <- compute 
              $ asBlockMap (warp_reduce f) 
              $ splitUp warp_th arr
      reduceKernel f arr' 

hybrid_reduce' :: Data a
              => Word32 
              -> (a -> a -> a)
              -> Pull Word32 a -> Push Block Word32 a
hybrid_reduce' warp_th f arr = exec $ b_body arr
  where
    b_body arr = do
      arr' <- compute 
              $ asBlockMap (exec . warp_reduce' f)
              $ splitUp warp_th arr
      reduceKernel f arr' 


  


reductions2 :: forall a . Data a
           => Word32 
           -> (a -> a -> a)
           -> Pull EWord32 (SPull a)
           -> Push Grid EWord32 a 
reductions2 warp_th f arr = asGridMap (hybrid_reduce' warp_th f) arr
  -- asGridMap body arr
   -- where
   --   body :: Pull Word32 a -> Push Block Word32 a
   --   body arr = execBlock $ doBody
   --     where doBody = 
   --             do arr' <- compute
   --                        $ asBlockMap wbody
   --                        $ splitUp warp_th arr
   --                reduceKernel f arr'

   --   wbody :: Pull Word32 a -> Push Warp Word32 a
   --   wbody arr = execWarp $ reduceKernel f arr 



genIt :: (ToProgram (Pull EWord32 a -> Push Grid EWord32 a), Data a)
         => String
         -> (Pull EWord32 (SPull a) -> Push Grid EWord32 a)
         -> Word32
         -> Word32
         -> IO () 
genIt name kernel threads size =
  putStrLn $
  genKernel threads name  
    (kernel . splitUp size) 


main = do
  genIt "reduce" (reductions2 32  (+) :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32) 128 128

  putStrLn "VARYING WARP_TH (threads:128)" 
  execIt (reductions2 32 (+)) 1024 2 128 2
  execIt (reductions2 64 (+)) 1024 2 128 2
  execIt (reductions2 128 (+)) 1024 2 128 2
  execIt (reductions2 256 (+)) 1024 2 128 2
  execIt (reductions2 512 (+)) 1024 2 128 2
  execIt (reductions2 1024 (+)) 1024 2 128 2
  
  putStrLn "OTHER THREADS SETTING (256)" 
  execIt (reductions2 32 (+)) 1024 2 256 2
  execIt (reductions2 64 (+)) 1024 2 256 2
  execIt (reductions2 128 (+)) 1024 2 256 2
  execIt (reductions2 256 (+)) 1024 2 256 2
  execIt (reductions2 512 (+)) 1024 2 256 2
  execIt (reductions2 1024 (+)) 1024 2 256 2

  putStrLn "OTHER THREADS SETTING (512)" 
  execIt (reductions2 32 (+)) 1024 2 512 2
  execIt (reductions2 64 (+)) 1024 2 512 2
  execIt (reductions2 128 (+)) 1024 2 512 2
  execIt (reductions2 256 (+)) 1024 2 512 2
  execIt (reductions2 512 (+)) 1024 2 512 2
  execIt (reductions2 1024 (+)) 1024 2 512 2

  putStrLn "STRANGE WARP SETTINGS (that does not properly divide size)"
  execIt (reductions2 37 (+)) 1024 2 256 2
  execIt (reductions2 61 (+)) 1024 2 256 2
  execIt (reductions2 165 (+)) 1024 2 256 2
  execIt (reductions2 294 (+)) 1024 2 256 2
  execIt (reductions2 458 (+)) 1024 2 256 2
  execIt (reductions2 759 (+)) 1024 2 256 2

  putStrLn "STRANGE WARP SETTINGS (that are more \"nice\")"
  execIt (reductions2 1 (+)) 1024 2 256 2
  execIt (reductions2 2 (+)) 1024 2 256 2
  execIt (reductions2 4 (+)) 1024 2 256 2
  execIt (reductions2 8 (+)) 1024 2 256 2
  execIt (reductions2 16 (+)) 1024 2 256 2
  execIt (reductions2 32 (+)) 1024 2 256 2

  


  
-- main = genIt "reduce" (reductions (+) :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32) 128 128

-----------------------------------------------------------------
-- Execute it
-----------------------------------------------------------------
execIt kernel chunk_size n_chunks threads blocks =
  withCUDA $
    do
      kern <- capture threads ( (kernel . splitUp chunk_size) :: Pull EWord32 EWord32 -> Push Grid EWord32 EWord32)

      useVector (V.fromList [0..(chunk_size * (fromIntegral n_chunks))::Word32]) $ \input ->
        withVector n_chunks $ \output ->
        do
          output <== (blocks, kern) <> input
          res <- peekCUDAVector output
          lift $ putStrLn $ show res 
                  
                              
      

