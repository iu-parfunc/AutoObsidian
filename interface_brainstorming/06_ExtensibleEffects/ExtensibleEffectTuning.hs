{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | The idea with this prototype is to use a monad, but accumulate a
-- type-level list of tuning decisions, indexed by unique names.

module ExtensibleEffectTuning
 where

import Control.Applicative
import Control.Monad.ST

-- import Data.Proxy
-- import System.Random.MWC
import GHC.TypeLits

import Control.Eff
import Data.Typeable

----------------------------------------
-- ^ A concise proxy type
-- data P (a :: k) = P

----------------------------------------

-- ^ This is nothing but a reader effect for an integral parameter
-- which is identified by the type-level Symbol.
data ParamRdr s a = KnownSymbol s => GetParm (Int -> a)
  deriving (Typeable)

instance KnownSymbol s => Functor (ParamRdr s) where
  fmap f (GetParm g) = GetParm (f . g)

getParam :: forall s r . (KnownSymbol s, Typeable s, Member (ParamRdr s) r) =>
            Proxy s -> Eff r Int
getParam _ = send (inj (GetParm id :: ParamRdr s Int))

runParamRdr :: forall s r w . (Typeable s) =>
               Proxy s -> (Int,Int) ->
               Eff (ParamRdr s :> r) w ->  Eff r w
runParamRdr _ bnds m  = loop m
  where
  loop = freeMap return
         (\u -> handleRelay u loop
           (\(GetParm f) -> loop (f (rng (symbolVal (Proxy::Proxy s)) bnds))))
  loop :: Free (Union (ParamRdr s :> r)) a -> Free (Union r) a

  rng "a" (x,_) = x
  rng "b" (_,y) = y
  rng  _  (x,y) = x+y `quot` 2

-- Note: Signature is inferrable.
example :: (Member (ParamRdr "a") r, Member (ParamRdr "b") r) => Eff r (Int,Int)
example =
  do x <- getParam (Proxy::Proxy "a")
     y <- getParam (Proxy::Proxy "b")
     return (x,y)


test :: (Int,Int)
test = run $ runParamRdr (Proxy::Proxy "a") (0,10) $
             runParamRdr (Proxy::Proxy "b") (10,20) $
             runParamRdr (Proxy::Proxy "c") (20,30) $ -- Ok to run with extra effects.
             example
