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
data Config result = Config { numBits   :: Int
                            , numParams :: Int
                            , numIters  :: Int
                            , coolRate  :: result
                            , initTemp  :: result
                            , verbose   :: Bool
                            }


acceptance :: (Num a, Ord a, Floating a)
           => a -> a -> a -> a
acceptance e e' t =
  if e' < e
  then 1.0
  else exp $ (e - e')/t

type SearchState result = ( StdGen
                          , MultiBitString
                          , result
                          , Maybe result
                          , ResultLog result
                          )

newtype SimulatedAnnealingSearch result a =
  SimulatedAnnealingSearch (ReaderT (Config result) (StateT (SearchState result) IO) a)
  deriving ( Monad
           , MonadIO
           , MonadState (SearchState result)
           , MonadReader (Config result)
           , Functor
           , Applicative
           )

instance (Ord result, Show result, Floating result, Random result)
         => SearchMonad result SimulatedAnnealingSearch where
  getParam i = do
    (_,bstr,_,_,_) <- get
    return $ bitStringToNum $ nthParam bstr i

runSearch :: (Show result, Ord result, Floating result, Random result)
          => (Config result)
          -> SimulatedAnnealingSearch result (Maybe result)
          -> IO (ResultLog result)
runSearch cfg (SimulatedAnnealingSearch m) = do
  stdGen <- newStdGen

  let (g',g'') = split stdGen
      init = makeIndividual (numBits cfg) (numParams cfg) g'

      -- Compare two results
      resComp p1@(Just res1, _) p2@(Just res2, _) t g =
        if res2 <= res1
        then p2
        else let (r,_) = randomR (0.0,1.0) g
                 prob = acceptance res1 res2 t
             in if r < prob
                then p2
                else p1
        -- if res1 <= res2 then p1 else p2
      resComp a _ _ _ = a

      testBit b p iter = do
        (g,bstr,temp,res,rlog) <- get
        let bstrNew = flipBitAt b p bstr
        put (g,bstrNew,temp,Nothing,rlog)
        resNew <- m
        let (g',g'') = split g
            (resBest,bstrBest) = resComp (resNew,bstrNew) (res,bstr) temp g'
        let rlog' =
              -- Only record in log if we move to the new solution
              case resNew of
               Nothing -> rlog
               Just r  -> if (resBest == resNew)
                          then addResult rlog r iter
                          else rlog
        put (g'',bstrBest,temp,resBest,rlog')
        if (verbose cfg)
          then do
          liftIO $ putStrLn $ "Current best: " ++ (show resBest)
          liftIO $ putStrLn $ "Moving state: " ++ (show $ resBest == resNew)
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
