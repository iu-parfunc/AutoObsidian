{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Bit climbing is hill climbing on bit strings.

module Auto.BitClimbSearch where

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import System.Random
import Data.Array
import Auto.SearchMonad
import Auto.ResultLog
import Auto.Score
import Prelude hiding (init)

-- We're using bitstrings to store numbers internally, so we need to
-- parameterize the search with the number of bits to use for our
-- parameters. This assumes each parameter has the same number of bits.
data Config = Config { numBits   :: Int
                     , numParams :: Int
                     , numIters  :: Int
                     , verbose   :: Bool
                     }

-- The search needs to keep track of the current bitstring
-- and maybe the evaluated result
type SearchState result = ( StdGen
                          , Array Int BitString
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

  -- I should clean up all the get/put stuff into something
  -- more straightforward. It's hard to follow as it is.

  getParam i = do
    (_,bstr,_,_) <- get
    return $ bitStringToNum $ bstr ! i

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

    (_,(_,_,_,rlog)) <- runStateT (runReaderT m' cfg)
                        (g'', init, Nothing,
                         ResultLog (mkFLIFO $ Just 10) (Just $ mkFLIFO Nothing))
    return $ rlog -- $ peek $ resultLogBest rlog
