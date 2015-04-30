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
  type SearchAux    ExhaustiveSearch = [Int] 

  getParam i = do
    (params,_) <- get

    -- Add error checking
    if (i > length params - 1 || i < 0)
      then error "Exhaustive: getParam"
      else return (params !! i)



  runSearch cfg (ExhaustiveSearch m) = do


    let m' combos = forM_ combos $ \params ->
          do
            (old_params,rlog) <- get
            put (params,rlog)

            m_res <- m

            let rlog' =
                  case m_res of
                    Nothing -> rlog
                    Just r  -> addResult rlog r

            put (old_params, rlog')

    let m'' = do cfg' <- ask
                 let combos = sequence $ paramLists cfg'
                 m' combos

    (_a,s) <- runStateT (runReaderT m'' cfg)
                        ( []
                        , ResultLog (mkFLIFO $ Just 10)
                                    (Just $ mkFLIFO Nothing))
    return s -- $ snd s -- $ peek (resultLogBest (snd s))
