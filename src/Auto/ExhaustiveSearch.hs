{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Auto.ExhaustiveSearch where

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative


import Auto.SearchMonad
import Auto.ResultLog


-- config for exhaustive search
data Config = Config { paramLists :: [[Int]] }


newtype ExhaustiveSearch result a =
  ExhaustiveSearch (ReaderT Config (StateT ([Int]
                                           , ResultLog result) IO) a) 
 deriving ( Monad
          , MonadIO 
          , MonadState ([Int], ResultLog result)
          , MonadReader Config
          , Functor
          , Applicative)


instance Ord result => SearchMonad result ExhaustiveSearch where
  type SearchConfig ExhaustiveSearch = Config

  getParam i = do
    (params,_) <- get
    -- Add error checking 
    return (params !! i)  
    
                  

  runSearch cfg (ExhaustiveSearch m) = do

    
    let m' combos= forM_ combos $ \params ->
          do
            -- remove best from state.. the log is enough 
            (old_params,rlog) <- get
            -- put (old_best,params) -- pointless
            
            m_res <- m 
    
            let rlog' =
                  case m_res of
                    Nothing -> rlog
                    Just r  -> addResult rlog r 

            put (old_params, rlog')
            
            -- case res of
            --   Nothing -> return ()  -- put (r,params) -- params are junk by now
            --   -- Just (r_params,r') ->
            --   Just res -> 

                
            --     case r of
            --       Nothing -> put (res, params) -- params are junk
            --       Just (_,old_r) -> if (r' < old_r)
            --                         then put (res,params)
            --                         else return ()
    let m'' = do cfg <- ask
                 let combos = sequence $ paramLists cfg
                 m' combos 
    
        
    (a,s) <- runStateT (runReaderT m'' cfg)
                       ( []
                       , ResultLog (mkFLIFO $ Just 10)
                                   (Just $ mkFLIFO Nothing))
    return $ peek (resultLogBest (snd s))
        
