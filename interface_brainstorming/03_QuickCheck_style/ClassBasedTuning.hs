{-# LANGUAGE ConstraintKinds #-}

-- | This version explores the simple option of using type-classes to
-- overload on the type of the tuning input parameters.  This enables
-- us to remain polymorphic in how many "dimensions" are being tuned.
-- It enables some forms of composability, but requires that the
-- dimensionality be known statically at type checking time.

module ClassBasedTuning where

import Test.QuickCheck

-- | The assessment of how good a run is; bigger is better.
type Score = Double

type Tunable t = Arbitrary t

-- Arbitrary is sufficient for tunable, but it could add more control as well...
--
-- class Arbitrary t => Tunable t where

-- | The tuner takes a tunable and returns the best tuning parameter
-- for it after expending a (currently nonconfigurable) amount of
-- effort.
tune :: Tunable t =>
        (a -> t -> (b,Score)) -> t
tune = undefined

-- | Online tuning returns a pure function
onlineTune :: Tunable t =>
              (a -> t -> (b,Score)) -> (a->b)
onlineTune = undefined

--------------------------------------------------

-- | Because we're overloaded in the type of the tunable, we can tune
-- over arbitrary-dimensional tuning spaces by just
example1 :: String -> (Int,Int) -> (String,Score)
example1 inp (i,j) = (inp++ " done", fromIntegral (i*j))

-- TODO/TOFIX: Without using a ton of newtypes, this doesn't provide a
-- way to bound the range of "Int" tuning parameters!

-- The "refined" package on hackage shows one way to do this:
--  https://hackage.haskell.org/package/refined

test :: (Int, Int)
test = tune example1
