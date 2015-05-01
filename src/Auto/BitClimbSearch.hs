{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- Bit climbing is hill climbing on bit strings.

{-|
Module      : Auto.BitClimbSearch
Description : BitClimbSearch instance for SearchMonad
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  :
Stability   : experimental
Portability :

Implementation of bit climb search instance for the SearchMonad class.

"Bit climbing" is just  hill climbing over bit strings. In this
implementation, the choice of a bit string as the data structure
used in the search state is hard-coded.

The basic strategy is to start with a state with a bit string, and
take "steps" by testing random permutations of the bit string
(flipping one bit at random), and "stepping" to a new state if it
is evaluated to be better than the current step. So, we always step
to better states, which places an emphasis on exploitation rather than
exploration, but the procedure will potentially test many bad
configurations during the random permutations (though these bad
configurations are not stepped to).
-}


module Auto.BitClimbSearch where

import Auto.BitString
import Auto.ResultLog
import Auto.SearchMonad
import Control.Applicative
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Prelude              hiding (init)

-- | We're using bitstrings to store numbers internally, so we need to
--   parameterize the search with the number of bits to use for our
--   parameters. This assumes each parameter has the same number of bits.
data Config = Config { numBits   :: Int
                     , numParams :: Int
                     , numIters  :: Int
                     , verbose   :: Bool
                     }

-- | The search needs to keep track of the current bitstring
--   and maybe the evaluated result
type SearchState result = ( StdGen
                          , MultiBitString
                          , Maybe result
                          , ResultLog result
                          )

newtype BitClimbSearch result a =
  BitClimbSearch (ReaderT Config (StateT (SearchState result) IO) a)
  deriving ( Monad
           , MonadIO
           , MonadState (SearchState result)
           , MonadReader Config
           , Functor
           , Applicative
           )

instance (Ord result, Show result) => SearchMonad result BitClimbSearch where
  type SearchConfig BitClimbSearch = Config
  type SearchAux    BitClimbSearch =
    (StdGen, MultiBitString) -- problem getting the Maybe result out here

  -- | Returns the nth param, which is represented by the nth element
  --   in the bitstring list in the search state.
  getParam i = do
    (_,bstr,_,_) <- get
    return $ bitStringToNum $ nthParam bstr i

  -- | Run the hill climbing search.
  runSearch cfg (BitClimbSearch m) = do
    stdGen <- newStdGen

    let (g', g'') = split stdGen
        init = makeIndividual (numBits cfg) (numParams cfg) g'

        -- Compare two results
        resComp p1@(Just res1, _) p2@(Just res2, _) =
          if res1 <= res2 then p1 else p2
        resComp a@(Just _, _) (Nothing,_) = a
        resComp (Nothing,_) a = a

        -- Try a mutation and keep it if it's better than the current result
        testBit b p = do
          (g,bstr,res,rlog) <- get
          let bstrNew = flipBitAt b p bstr
          put (g,bstrNew,Nothing,rlog)
          resNew <- m
          let (resBest,bstrBest) = resComp (resNew,bstrNew) (res,bstr)
          put (g,bstrBest,resBest,rlog)
          if (verbose cfg)
            then do
            liftIO $ putStrLn $ "Current best: " ++ (show resBest)
            liftIO $ putStrLn $ "Moving state: " ++ (show $ resBest == resNew)
            return ()
            else
            return ()

        m' = forM_ [1..(numIters cfg)] $ \_experimentNum -> do
          (g,bstr,res,rlog) <- get
          let (r1,g1)  = randomR (0,(numBits cfg)-1) g
              (r2,g2) = randomR (0,(numParams cfg)-1) g1
          put (g2,bstr,res,rlog)
          testBit r1 r2
          return ()

    (_,(stdg,a,_,rlog)) <- runStateT (runReaderT m' cfg)
                        (g'', init, Nothing,
                         ResultLog (mkFLIFO $ Just 10) (Just $ mkFLIFO Nothing))
    return $ ((stdg,a),rlog) -- peek $ resultLogBest rlog
