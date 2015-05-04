{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- Genetic algorithm implementation

{-|
Module      : Auto.GeneticSearch
Description : The genetic algorithm implementation
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  :
Stability   : experimental
Portability :

An instance of SearchMonad for Holland's simple genetic algorithm.
-}

module Auto.GeneticSearch where

-- Ideally we could make this search only use the Genome typeclass,
-- rather than the BitString record type directly. I think that would
-- require some changes to the SearchMonad.

import Auto.BitString
import Auto.ResultLog
import Auto.SearchMonad
import Control.Applicative
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.List            (sort, sortBy)
import Data.Maybe           (fromJust, isJust)

-- | Configuration details for the search.
data Config = Config { numBits   :: Int
                     , numParams :: Int
                     , popSize   :: Int
                     , numIters  :: Int
                     , mutProb   :: Double
                     , tournSize :: Int
                     , verbose   :: Bool
                     }

-- | Tournament selection is a typical selection approach for genetic
--   algorithms. It runs little competitions among individuals randomly
--   picked from the population, and the two fittest in each contest
--   are subject to crossover to create a new individual in the next
--   generation.
--   Invariant: The population and result list must be the same length!
tournament
  :: (RandomGen g, Ord result)
  => Population
  -> [result]
  -> Int
  -> Rand g Population
tournament ps rs i =
  let num1 = length ps
      num2 = length rs
  in if num1 /= num2
     then error "Population size and result list length not equal!"
     else
       let produceOffspring _ = do
             -- Maybe we should ensure these are unique random values
             contestantInds <- getRandomRs (0,num1)
             let contestants = map (\i -> (ps !! i, rs !! i))
                             $ take i contestantInds
                 ranked = sortBy rankFunc contestants
                 rankFunc (_,r1) (_,r2) = compare r1 r2
                 [(bstr1,_),(bstr2,_)] = take 2 ranked
             splitPoint <- getRandomR (0,len $ bstr1 !! 0)
             let zipCrossFunc b1 b2 = crossBitString b1 b2 splitPoint
             return $ zipWith zipCrossFunc bstr1 bstr2
       in mapM produceOffspring ps

-- | Mutation is a random process that goes through the genomes of
--   individuals in our population and messes with stuff. We need
--   a parameter representing a probability between 0 and 1 of
--   how likely it is we mutate an individual bit.
mutation
  :: (Fractional f, Ord f, Random f, RandomGen g)
  => Population
  -> f
  -> Rand g Population
mutation ps prob =
  forM ps $ \mbs -> do
    forM mbs $ \b -> do
      flip <- getRandomR (0.0,1.0)
      if flip < prob
        then do bit <- getRandomR (0,len b)
                return $ mutateBitString b bit
        else return b

-- | This function launches the tournament and mutation functions,
--   given a population, a list of results, a mutation probability,
--   and a tournament size.
--   It also strips the Maybe off the results.
generation
  :: (Ord result, Fractional f, Ord f, Random f, RandomGen g)
  => Population
  -> [Maybe result]
  -> f
  -> Int
  -> Rand g Population
generation ps mrs prob size = do
  -- I'm not sure what to do with Nothings here, since we don't know how
  -- much of a penalty to apply to an arbitrary result type. This needs to
  -- be revisited.
  rs <- forM mrs $ \mr -> do
    case mr of
     Just r -> return r
     Nothing -> error "Result not evaluated"
  ps <- tournament ps rs size
  ps <- mutation ps prob
  return ps

-- | For the internal state, we need a list of individuals (population)
--   as well as a currently selected individual. This is
--   represented as an integer index. We also need to keep track of the
--   evaluation scores of the population, which may be absent or out of
--   date, or may represent an invalid individual genome, so they're
--   individually wrapped in Maybe.
type SearchState result = ( Population
                          , Int
                          , [Maybe result]
                          , ResultLog result
                          )


newtype GeneticSearch result a =
  GeneticSearch (ReaderT Config (StateT (SearchState result) IO) a)
  deriving ( Monad
           , MonadIO
           , MonadState (SearchState result)
           , MonadReader Config
           , Functor
           , Applicative
           )

instance (Ord result, Show result) => SearchMonad result GeneticSearch where
  type SearchConfig GeneticSearch = Config
  type SearchAux    GeneticSearch = Population

  -- To get the requested parameter we have to look up which thing
  -- we're evaluating in the state. This feels very un-functional to me.

  getParam i = do
    (bstr,n,_,_) <- get
    return $ bitStringToNum $ nthIndividualParam bstr n i

  -- runSearch cfg (GeneticSearch m) = undefined
  runSearch cfg (GeneticSearch m) = do
    stdGen <- newStdGen -- we should be threading this through the search

    let initFunc 0 _ = []
        initFunc i g =
          let (g',g'') = split g
              newI = makeIndividual (numBits cfg) (numParams cfg) g'
          in newI : (initFunc (i-1) g'')
        initPop = initFunc (popSize cfg) stdGen

        recordBest = do
          (p,ind,rs,rlog) <- get
          let results = map fromJust $ filter isJust rs
              best = head $ sort results
              rlog' = addResult rlog best 1 -- HACK HACK HACK !!!! 
          put (p,ind,rs,rlog')
          if (verbose cfg)
            then do liftIO $ putStrLn $ "Best in generation: " ++ (show best)
                    return ()
            else return ()

        evalPop = do
          rs <- forM [0..(popSize cfg)] $ \ind -> do
            (p,_,rs,rlog) <- get
            put (p,ind,rs,rlog)
            score <- m
            return score
          (p,ind,_,rlog) <- get
          put (p,ind,rs,rlog)

        handlePop = do
          (p,ind,rs,rlog) <- get
          -- this should be prettier...
          -- using io just for random numbers is silly
          p' <- liftIO $ evalRandIO $ generation p rs (mutProb cfg) (tournSize cfg)
          put (p',ind,rs,rlog)
          evalPop
          recordBest
          return ()

        m' = forM_ [1..(numIters cfg)] $ \_iter -> do
          (_,_,rs,_) <- get
          if null rs
            then do evalPop
                    handlePop
                    return ()
            else do handlePop
                    return ()

    (_,(p,_,_,rlog)) <- runStateT (runReaderT m' cfg)
                         (initPop,0,[],
                          ResultLog (mkFLIFO $ Just 10)
                                    (Just $ mkFLIFO Nothing)
                                    [])
    return (p,rlog)
