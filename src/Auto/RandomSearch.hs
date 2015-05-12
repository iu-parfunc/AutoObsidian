{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


{-|
Module      : Auto.RandomSearch
Description : RandomSearch instance for SearchMonad 
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  : 
Stability   : experimental
Portability : 

Implementation of random search instance for the SearchMonad class. 
-}
module Auto.RandomSearch where


import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import System.Random (randomR, StdGen, newStdGen)

import Auto.SearchMonad
import Auto.ResultLog

-- | Config for Random search
--   Configuration consists of a list of ranges, one range per parameter
--   being tuned over, and an integer specifying how many configurations
--   should be tested. 
data Config = Config { paramRanges :: [(Int,Int)]
                     , numIters :: Int }

newtype RandomSearch result a =
  RandomSearch (ReaderT Config (StateT ( StdGen
                                       , ResultLog result)  IO)  a)
 deriving ( Monad
          , MonadIO
          , MonadState (StdGen, ResultLog result)
          , MonadReader Config
          , Functor
          , Applicative)

instance Ord result => SearchMonad result RandomSearch where
  getParam i = do
    cfg <- ask
    --- Here ---
    (g,r) <- get
    -- Add error checking !
    let range = (paramRanges cfg) !! i
        (a,g') = randomR range g
    put (g',r)
    --- To here -- can be replaced with a modify
    return a

runSearch :: Ord result => Config
          -> RandomSearch result (Maybe result)
          -> IO (ResultLog result)
runSearch cfg (RandomSearch m) = do
    stdGen <- newStdGen -- splits some "global" generator
  
  
    -- number of runs is now configurable
    let m' = forM_ [1..(numIters cfg)] $ \iter ->
          -- experiment_num could be used for something (info printing)
          do

            m_res <- m

            (g,rlog) <- get

            let rlog' =
                  case m_res of
                    Nothing -> rlog
                    Just r -> addResult rlog r iter

            put (g,rlog')

    (_a,s) <- runStateT (runReaderT m' cfg)
                        ( stdGen
                       , ResultLog (mkFLIFO $ Just 10)
                                   (Just $ mkFLIFO Nothing)
                                   [] )
    return $ snd s -- $ peek (resultLogBest (snd s))
