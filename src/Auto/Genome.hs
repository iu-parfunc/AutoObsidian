{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Auto.Genome
Description : The genetic algorithm implementation
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  :
Stability   : experimental
Portability :

A typeclass for basic operations on genomes.
-}

module Auto.Genome (Genome(..)) where

import Control.Monad.Random

-- | A genome is parameterized by its representation and its
--   "return value" (i.e. what type of value it represents).
--   You can expect to mutate and cross over genomes.
class Genome a b where

  -- | Convert a value to a genome.
  toValue :: b -> a

  -- | Randomly mutate a genome based on some probability.
  mutate :: (RandomGen g, Fractional f, Ord f, Random f)
         => g -> f -> b -> b

  -- | Cross over two genomes.
  cross :: (RandomGen g) => g -> b -> b -> b
