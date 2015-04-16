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
import System.Random (StdGen, newStdGen, randomRs, split)
import Data.Array
import Auto.SearchMonad
import Auto.ResultLog
import Auto.Score

-- We're using bitstrings to store numbers internally, so we need to
-- parameterize the search with the number of bits to use for our
-- parameters. This assumes each parameter has the same number of bits.
data Config = Config { numBits   :: Int
                     , numParams :: Int
                     , numIters  :: Int
                     }

-- The search needs to keep track of the current bit
type SearchState result = ( StdGen
                          , Array Int BitString
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

instance Ord result => SearchMonad result BitClimbSearch where
  type SearchConfig BitClimbSearch = Config

  getParam i = do
    (_,bstr,_) <- get
    return $ bitStringToNum $ bstr ! i

  runSearch cfg (BitClimbSearch m) = do
    stdGen <- newStdGen
    let (g', g'') = split stdGen
        init = makeIndividual (numBits cfg) (numParams cfg) g'
        setBit b p = do
          (g,bstr,rlog) <- get
          let bstr' = flipBitAt b p bstr
          put (g,bstr',rlog)
          return ()
        testBit b p = do
          (g,bstr,rlog) <- get
          setBit b p
          mRes <- m
          put (g,bstr,rlog)
          return mRes
        -- Maybe rewrite the following to use a single test per
        -- experiment, and choose the bit to consider randomly. 
        m' = forM_ [1..(numIters cfg)] $ \_experimentNum -> do
          -- Should abort when we can't improve the current answer
          res <- forM [(b,p) |
                       b <- [0..(numBits cfg)-1],
                       p <- [0..(numParams cfg)-1]]
                 $ \(b,p) -> do
            -- Currently searches all neighbors. Instead it should stop
            -- when it finds one that is better than the current state.
            mRes <- testBit b p
            return (mRes,b,p)
          (g,bstr,rlog) <- get
          return ()
          -- let rlog' = case mRes of
          --       Nothing -> rlog
          --       Just r  -> addResult rlog r
          -- put (g,bstr,rlog')
    (_,(_,_,rlog)) <- runStateT (runReaderT m' cfg)
                      (g'', init, ResultLog (mkFLIFO $ Just 10) (Just $ mkFLIFO Nothing))
    return $ peek $ resultLogBest rlog
