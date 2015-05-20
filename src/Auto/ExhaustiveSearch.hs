{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Auto.ExhaustiveSearch
Description : ExhaustiveSearch instance for SearchMonad 
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  : 
Stability   : experimental
Portability : 

Implementation of exhaustive search instance for the SearchMonad class. 
-}
module Auto.ExhaustiveSearch where

import Control.Monad.State
import Control.Applicative


import Auto.SearchMonad
import Auto.ResultLog


-- config for exhaustive search
-- | Configuration for exhaustive search contains a
--   a list of parameter settings per parameter being tuned over. 
data Config = Config { paramLists :: [[Int]] }


newtype ExhaustiveSearch result a =
  ExhaustiveSearch (StateT ([Int]
                           , ResultLog result) IO a)
 deriving ( Monad
          , MonadIO
          , MonadState ([Int], ResultLog result)
--           , MonadReader Config
          , Functor
          , Applicative)


instance Ord result => SearchMonad (ExhaustiveSearch result) where
  getParam i = do
    (params,_) <- get

    -- Add error checking
    if (i > length params - 1 || i < 0)
      then error "Exhaustive: getParam"
      else return (params !! i)


-- | Run exhaustive search 
runSearch :: Ord result => Config
          -> ExhaustiveSearch result (Maybe result)
          -> IO (ResultLog result)
runSearch cfg (ExhaustiveSearch m) = do
  let m' combos = forM_ (zip combos [1..]) $ \(params,iter) ->
        do
          (old_params,rlog) <- get
          put (params,rlog)

          m_res <- m

          let rlog' =
                case m_res of
                  Nothing -> rlog
                  Just r  -> addResult rlog r iter 

          put (old_params, rlog')

  let m'' = do let combos = sequence $ paramLists cfg
               m' combos

  (_a,s) <- runStateT m''
                      ( []
                      , ResultLog (mkFLIFO $ Just 10)
                                  (Just $ mkFLIFO Nothing)
                                  [])
  return $ snd s -- $ peek (resultLogBest (snd s))

