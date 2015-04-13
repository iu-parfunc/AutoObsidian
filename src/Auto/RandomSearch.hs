{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-} 

module Auto.RandomSearch where


import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative


import System.Random (randomR, StdGen, newStdGen)

import Auto.SearchMonad 

-- Config for Random search 
data Config = Config { paramRanges :: [(Int,Int)]
                     , numIters :: Int }

newtype RandomSearch a =
  RandomSearch (ReaderT Config (StateT (Result,StdGen)  IO)  a) 
 deriving ( Monad
          , MonadIO 
          , MonadState (Result, StdGen)
          , MonadReader Config
          , Functor
          , Applicative)

instance SearchMonad RandomSearch where
  type SearchConfig RandomSearch = Config
  getParam i = do
    cfg <- ask
    --- Here --- 
    (r,g) <- get
    -- Add error checking ! 
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
