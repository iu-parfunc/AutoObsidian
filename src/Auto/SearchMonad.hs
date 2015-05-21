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

Using the SearchMonad to tune something requires making a
monadic action that will act as a fitness function. Inside
the action, you can query for parameters.
-}

{-

TODOs for the SearchMonad abstraction:

 * Refactor handling of random numbers

 * Refactor as monad transformer

 * Further abstract parameters and data representation

-}

module Auto.SearchMonad
       ( SearchMonad(..)
       , module Control.Monad  -- get access to forM_ etc
       , module Control.Monad.IO.Class -- maybe just liftIO ?
       ) where

-- These are used by our apps and instances but really have no
-- place being here! 
import Control.Monad
import Control.Monad.IO.Class

-- import Auto.ResultLog

-- List of parameters and score
-- TODO: Abstract this further
-- It allows only Int parameters

-- | SearchMonad is parameterised over result (fitness) type.
--   Rather than a fitness function, a monadic action is performed
--   to evaluate candidate solutions.
class SearchMonad m where
  -- | Get one of the parameters being tuned. Should be called from
  --   the evaluation action.
  getParam :: Int -> m Int


-- This change makes the "IO" aspect of the interface more strange to
-- me Now it up to any instance of SearchMonad to decide if it is
-- MonadIO or not for itself 
