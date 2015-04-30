{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE TypeSynonymInstances -}
{- LANGUAGE FlexibleInstances -}
{- LANGUAGE GeneralizedNewtypeDeriving -}
{-# LANGUAGE TypeFamilies #-} 


{- TODOs
   # Further abstractions
     Parameter types
     result types (should just require an Ord instance) 

-} 

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
class ( Ord result
      , Monad (m result) ) => SearchMonad result m where
  
  type SearchConfig m
  type SearchAux    m 

  -- | Run a search to obtain a full resultlog together
  --   with any Auxiliary data a specific instance decides to return.
  runSearch :: SearchConfig m
            -> m result (Maybe result)
            -> IO (SearchAux m, ResultLog result)

  -- | get one of the parameters being tuned over.
  getParam :: Int -> m result Int


-- | similar to runSearch but does not return any Auxiliary data. 
execSearch :: SearchMonad result m
           => SearchConfig m
           -> m result (Maybe result)
           -> IO (ResultLog result)
execSearch conf m = do
  (_,rlog) <- runSearch conf m 
  return rlog 
