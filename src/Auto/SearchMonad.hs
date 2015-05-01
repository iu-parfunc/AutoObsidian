{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-|
Module      : Auto.SearchMonad
Description : The SearchMonad class
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  :
Stability   : experimental
Portability :

This module contains the SearchMonad class and directly
related functions.
-}

{-

TODOs for the SearchMonad abstraction:

 * Further abstractions

 * Parameter types

 * result types (should just require an Ord instance)

-}

module Auto.SearchMonad
       ( SearchMonad(..)
       , execSearch
       , module Control.Monad  -- get access to forM_ etc
       , module Control.Monad.IO.Class -- maybe just liftIO ?
       ) where

import Control.Monad
import Control.Monad.IO.Class

import Auto.ResultLog

-- List of parameters and score
-- TODO: Abstract this further
-- It allows only Int parameters

-- | SearchMonad is parameterised over result (fitness) type.
--   Rather than a fitness function, a monadic action is performed
--   to evaluate candidate solutions.
--   Individual search strategies implement this class, and specify
--   their own SearchConfig and SearchAux.
class (Ord result, Monad (m result)) => SearchMonad result m where

  type SearchConfig m -- ^ The configuration settings for a particular search.

  type SearchAux    m -- ^ Values returned in addition to the result.

  -- | Run a search to obtain a full resultlog together
  --   with any Auxiliary data a specific instance decides to return.
  runSearch :: SearchConfig m
            -> m result (Maybe result)
            -> IO (SearchAux m, ResultLog result)

  -- | Get one of the parameters being tuned. Should be called from
  --   the evaluation action.
  getParam :: Int -> m result Int


-- | Similar to runSearch but discards the SearchAux data and just
--   returns the result.
execSearch :: SearchMonad result m
           => SearchConfig m
           -> m result (Maybe result)
           -> IO (ResultLog result)
execSearch conf m = do
  (_,rlog) <- runSearch conf m
  return rlog
