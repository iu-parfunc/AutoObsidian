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
-- import GHC.TypeLits

import Control.Eff
import Data.Typeable

----------------------------------------

data RandomNumEff a = GetParm Int Int (Int -> a)
  deriving (Typeable, Functor)

getParam :: (Member RandomNumEff r) => Int -> Int -> Eff r Int
getParam n m = send (inj (GetParm n m id))

runRandomNumEff :: Eff (RandomNumEff :> r) w -> ((Int,Int) -> Int) -> Eff r w
runRandomNumEff m rng = loop m
  where
  loop = freeMap return
         (\u -> handleRelay u loop
           (\(GetParm x y f) -> loop (f (rng (x,y)))))

  -- loop :: Free (Union (RandomNumEff :> r)) a -> Free (Union r) a

example :: (Member RandomNumEff r) => Eff r (Int,Int)
example =
  do x <- getParam 0 10
     y <- getParam 10 20
     return (x,y)

test :: (Int,Int)
test = run $ runRandomNumEff example fst



--------------------------------------------------------------------------------

-- runFoo = handle_relay return (\FooChoice k -> getParam 10 20 >>= unArr k)
