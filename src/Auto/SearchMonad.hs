{-# LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE MultiParamTypeClasses -}
{- LANGUAGE TypeSynonymInstances -}
{- LANGUAGE FlexibleInstances -}
{- LANGUAGE GeneralizedNewtypeDeriving -}
{-# LANGUAGE TypeFamilies #-} 


{- TODOs
   # Further abstractions
     Parameter types
     result types (should just require an Ord instance) 

-} 

module Auto.SearchMonad
       ( Result(..)
       , SearchMonad(..)
       , module Control.Monad  -- get access to forM_ etc
       , module Control.Monad.IO.Class -- maybe just liftIO ? 
       ) where

import Control.Monad
import Control.Monad.IO.Class

-- List of parameters and score
-- TODO: Abstract this further
-- It allows only Int parameters and Double results...
type Result = Maybe ([Int],Double)

class (Monad m, MonadIO m) => SearchMonad m where
  type SearchConfig m 
  runSearch :: SearchConfig m -> m (Result) -> IO Result   
  getParam :: Int -> m Int

