{-# LANGUAGE NamedFieldPuns #-}
-- | This demonstrates how to use Applicative rather than Monad to
-- provide a Reader-like construct for accessing tunable parameters.
-- Using Applicative means that we can ascertain exactly what tunables
-- are used *before* running the computation.

module ApplicativeTuning
 (
   -- * The abstract datatype for tunable computations
   Tune
 , allParams
 , getParam
   -- * Running and searching
 , tune
 , runMin
   -- * Temporary
 , test
 ) where

import Control.Applicative
import Data.List

-- | We restrict tunable parameters to Int values for now.
type Params = [Int]

type Score = Double

-- | The datatype for (auto)tunable computations.
data Tune a =
     Tune { run :: (Params -> a)
            -- ^ Run a tunable computation
          , allParams :: [Range] }

instance Show (Tune a) where
  show (Tune {allParams}) =
    "<Tunable computation with params "++show allParams++">"

instance Functor Tune where
  fmap f (Tune g l) = Tune (f . g) l

instance Applicative Tune where
  pure x = Tune (\[] -> x) []
  Tune f l1 <*> Tune g l2 =
    let len1 = length l1
    in Tune (\ls -> let (x,y) = splitAt len1 ls
                    in f x (g y)
            )
            (l1 ++ l2)

-- | Run a tunable computation and retrieve the best found tuning
-- parameters and their score.  We also get the result!
tune :: Tune a -> (a, Params, Score)
tune = undefined

-- | A quick way to run with all the *minimum* parameter settings,
-- i.e. without performinging any tuning.
runMin :: Tune a -> a
runMin (Tune f ls) = f (map fst ls)

-- | Fetch an integer in the range
getParam :: Range -> Tune Int
getParam r = Tune (\[p] -> p) [r]

-- | Inclusive ranges
type Range = (Int,Int)


example1 :: Tune Int
example1 =
  (-) <$> getParam (1,10) <*> getParam (0,3)

example2 :: Tune Double
example2 =
  (\x y -> fromIntegral (x+y)) <$> getParam (11,12) <*> getParam (13,14)

-- Illustrate composability:
example3 :: Tune (Int,Double)
example3 =
  (,) <$> example1 <*> example2

test :: Integer
test = 3
