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
-- It allows only Int parameters and Double results...

-- TODO: I think that runSearch
--       should return a full resultLog!

class ( Ord result
      , Monad (m result)
      , MonadIO (m result) ) => SearchMonad result m where
  
  type SearchConfig m
  type SearchAux    m 
  
  runSearch :: SearchConfig m
            -> m result (Maybe result)
            -> IO (SearchAux m, ResultLog result)
            
  getParam :: Int -> m result Int



execSearch :: SearchMonad result m
           => SearchConfig m
           -> m result (Maybe result)
           -> IO (ResultLog result)
execSearch conf m = do
  (_,rlog) <- runSearch conf m 
  return rlog 
