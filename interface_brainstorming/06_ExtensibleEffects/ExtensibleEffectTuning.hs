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
 -- (
 --   -- * The abstract datatype for tunable computations
 --   Tune
 -- , allParams
 -- , getParam
 --   -- * Running and searching
 -- , tune
 -- , tunePure
 -- , runMin
 --   -- * Temporary
 -- , test
 --
 -- )
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

-- data RandomNumEff (s :: Symbol) a =
data RandomNumEff s a =
     KnownSymbol s =>
     GetParm { bnds  :: (Int,Int) -- ^ inclusive
             , runit :: (Int -> a)
             }
  deriving (Typeable)

instance KnownSymbol s => Functor (RandomNumEff s) where
  fmap f (GetParm b g) = GetParm b (f . g)

getParam :: forall s r . (KnownSymbol s, Typeable s, Member (RandomNumEff s) r) =>
            Proxy s -> Int -> Int -> Eff r Int
getParam _ n m = send (inj (GetParm (n,m) id :: RandomNumEff s Int))

runRandomNumEff :: forall s r w . (Typeable s) =>
                   Proxy s ->
                   (String -> (Int,Int) -> Int) ->
                    Eff (RandomNumEff s :> r) w ->  Eff r w
runRandomNumEff _ rng m  = loop m
  where
  loop = freeMap return
         (\u -> handleRelay u loop
           (\(GetParm b f) -> loop (f (rng (symbolVal (Proxy::Proxy s)) b))))
  loop :: Free (Union (RandomNumEff s :> r)) a -> Free (Union r) a

example :: (Member (RandomNumEff "a") r,
            Member (RandomNumEff "b") r) =>
           Eff r (Int,Int)
example =
  do x <- getParam (Proxy::Proxy "a") 0 10
     y <- getParam (Proxy::Proxy "b") 10 20
     return (x,y)

{-

data A
data B

example2 :: (Member (RandomNumEff A) r,
             Member (RandomNumEff B) r) =>
            Eff r (Int,Int)
example2 =
  do x <- getParam (Proxy::Proxy A) 0 10
     y <- getParam (Proxy::Proxy ) 10 20
     return (x,y)
-}

test :: (Int,Int)
test = run $ runRandomNumEff (Proxy::Proxy "a") f $
             runRandomNumEff (Proxy::Proxy "b") f $
             example
 where
  f "a" (x,_) = x
  f "b" (_,y) = y




--------------------------------------------------------------------------------

-- runFoo = handle_relay return (\FooChoice k -> getParam 10 20 >>= unArr k)
