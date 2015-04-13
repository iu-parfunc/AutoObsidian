{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-} 


module Auto.ExhaustiveSearch where

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative


import Auto.SearchMonad


-- config for exhaustive search
data Config = Config { paramLists :: [[Int]] }


newtype ExhaustiveSearch a =
  ExhaustiveSearch (ReaderT Config (StateT (Result, [Int]) IO) a) 
 deriving ( Monad
          , MonadIO 
          , MonadState (Result, [Int])
          , MonadReader Config
          , Functor
          , Applicative)


instance SearchMonad ExhaustiveSearch where
  type SearchConfig ExhaustiveSearch = Config

  getParam i = do
    (r,params) <- get
    -- Add error checking 
    return (params !! i)  
    
                  

  runSearch cfg (ExhaustiveSearch m) = do

    
    let m' combos= forM_ combos $ \params ->
          do
            (r,old_params) <- get
            put (r,params)

            res <- m 
    
            case res of
              Nothing -> put (r,params) -- params are junk by now
              Just (r_params,r') ->
                case r of
                  Nothing -> put (res, params) -- params are junk
                  Just (_,old_r) -> if (r' < old_r)
                                    then put (res,params)
                                    else return ()
    let m'' = do cfg <- ask
                 let combos = sequence $ paramLists cfg
                 m' combos 
    
        
    (a,s) <- runStateT (runReaderT m'' cfg) (Nothing, [])
    return $ fst s 
        
