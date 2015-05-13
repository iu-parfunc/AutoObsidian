{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- NOT TESTED, PROBABLY DOESN'T WORK

{-|
Module      : Auto.SimulatedAnnealingSearch
Description : The simulated annealing implementation
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  :
Stability   : experimental
Portability :

An instance of SearchMonad for simulated annealing.
-}

module Auto.SimulatedAnnealingSearch where

import Auto.BitString
import Auto.ResultLog
import Auto.SearchMonad
import Control.Applicative
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

-- | Configuration details for the search.
data Config = Config { numBits   :: Int
                     , numParams :: Int
                     , numIters  :: Int
                     , coolRate  :: Double
                     , initTemp  :: Double
                     , scale     :: Double
                     , stride    :: Int
                     , verbose   :: Bool
                     }

class Annealable a where
  extract :: a -> Double


acceptance :: Double -> Double -> Double -> Double -> Double
acceptance e e' t s =
  if e' < e
  then 1.0
  else exp $ ((e - e') * s)/t

type SearchState result = ( StdGen
                          , MultiBitString
                          , Double
                          , Maybe result
                          , ResultLog result
                          )

newtype SimulatedAnnealingSearch result a =
  SimulatedAnnealingSearch (ReaderT Config (StateT (SearchState result) IO) a)
  deriving ( Monad
           , MonadIO
           , MonadState (SearchState result)
           , MonadReader Config
           , Functor
           , Applicative
           )

instance (Ord result, Show result, Annealable result)
         => SearchMonad result SimulatedAnnealingSearch where
  getParam i = do
    (_,bstr,_,_,_) <- get
    cfg <- ask
    return $ (stride cfg) * (bitStringToNum $ nthParam bstr i)

runSearch :: (Show result, Ord result, Annealable result)
          => Config
          -> SimulatedAnnealingSearch result (Maybe result)
          -> IO (ResultLog result)
runSearch cfg (SimulatedAnnealingSearch m) = do
  stdGen <- newStdGen

  let (g',g'') = split stdGen
      init = makeIndividual (numBits cfg) (numParams cfg) g'

      -- Compare two results
      resComp p1@(Just res1, _) p2@(Just res2, _) t s g =
        let (r,_) = randomR (0.0,0.9) g
            prob = acceptance (extract res1) (extract res2) t s
        in if r < prob
           then p2
           else p1
      resComp a@(Just _,_) _ _ _ _ = a
      resComp _ a@(Just _,_) _ _ _ = a
      resComp a _ _ _ _ = a

      testBit b p iter = do
        (g,bstr,temp,res,rlog) <- get
        let bstrNew = flipBitAt b p bstr
        put (g,bstrNew,temp,Nothing,rlog)
        resNew <- m
        let (g',g'') = split g
            (resBest,bstrBest) = resComp (res,bstr) (resNew,bstrNew) temp (scale cfg) g'
        let rlog' =
              case resNew of
               Nothing -> rlog
               Just r  -> addResult rlog r iter
        put (g'',bstrBest,temp,resBest,rlog')
        if (verbose cfg)
          then do
          liftIO $ putStrLn $ "Current best: " ++ (show resBest)
          liftIO $ putStrLn $ "Moving state: " ++ (show $ resBest == resNew)
          liftIO $ putStrLn $ "Current temp: " ++ (show temp)
          return ()
          else
          return ()

      m' = forM_ [1..(numIters cfg)] $ \iter -> do
        (g,bstr,temp,res,rlog) <- get
        let (r1,g1) = randomR (0,(numBits cfg)-1) g
            (r2,g2) = randomR (0,(numParams cfg)-1) g1
            newTemp = temp * (1 - (coolRate cfg))
        put (g2,bstr,newTemp,res,rlog)
        testBit r1 r2 iter
        return ()

  (_,(_,_,_,_,rlog)) <- runStateT (runReaderT m' cfg)
                      (g'', init, (initTemp cfg), Nothing,
                       ResultLog (mkFLIFO $ Just 10)
                                 (Just $ mkFLIFO Nothing)
                                 [] )

  return $ rlog -- ((stdg,a),rlog) -- peek $ resultLogBest rlog
