{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Auto.Search where

import Control.Monad.Identity (Identity(..))
import Data.List (foldl')
import System.Random (mkStdGen, random)

-- import GA (Entity(..), GAConfig(..), evolve)

-- decToBin x = reverse $ decToBin' x
--   where
--     decToBin' 0 = []
--     decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

-- binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l

-- type Number   = [Int]
-- type Sentence = Number
-- type Target   = Number
  
