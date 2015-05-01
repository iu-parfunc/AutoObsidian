{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Auto.BitString
       (
         BitString(..)
       , MultiBitString
       , Population
       , bitStringToNum
       , numToBitString
       , makeIndividual
       , makePopulation
       , flipBitAt
       , nthParam
       , nthIndividual
       , nthIndividualParam
       , crossBitString
       , mutateBitString
       )
       where

import Auto.Genome
import Control.Monad.Random
import Data.Array.Unboxed

data BitString =
  BitString
  { str :: UArray Int Bool
  , len :: Int
  }

instance Genome Int BitString where
  toValue = bitStringToNum
  mutate  = mutateBitStringGenome
  cross   = crossBitStringGenome

type MultiBitString = [BitString]

type Population = [MultiBitString]

-- Make a bitstring of size i.
makeBitString :: Int -> BitString
makeBitString i =
  BitString {
    str = array (0,i-1) [(j,False) | j <- [0..i-1]],
    len = i
    }

-- Convert bitstring to number.
bitStringToNum :: BitString -> Int
bitStringToNum (BitString {str}) = go $ assocs str
  where go [] = 0
        go ((_,False):xs) = go xs
        go ((i,True):xs)  = (2^i) + (go xs)

-- Flip bit at i in bitstring.
mutateBitString :: BitString -> Int -> BitString
mutateBitString (BitString {str,len}) i = BitString str' len
  where b = not $ str ! i
        str' = str // [(i,b)]

mutateBitStringGenome :: (RandomGen g, Fractional f, Ord f, Random f)
                      => g -> f -> BitString -> BitString
mutateBitStringGenome g prob bstr@(BitString _ i) =
  if flip < prob
  then mutateBitString bstr bit
  else bstr
  where (bit,_)   = randomR (0,i) g'
        (flip,g') = randomR (0.0,1.0) g

-- Crossover bitstrings arr1 and arr2.
crossBitString :: BitString -> BitString -> Int -> BitString
crossBitString (BitString {str=arr1}) (BitString {str=arr2}) i =
  BitString (array s l) i
  where s = bounds arr1
        l = zipWith f (assocs arr1) (assocs arr2)
        f (i1,v1) (_,v2)
          | (i1 < i)  = (i1,v1)
          | otherwise = (i1,v2)

crossBitStringGenome :: (RandomGen g)
                     => g -> BitString -> BitString -> BitString
crossBitStringGenome g bstr1 bstr2 =
  crossBitString bstr1 bstr2 point
  where (point,_) = randomR (0, len bstr1) g

-- Convert int to list of 0s and 1s.
numToBits :: Int -> [Int]
numToBits 0 = []
numToBits y = let (a,b) = quotRem y 2 in [b] ++ numToBits a

-- Make bitstring of number i and length s.
numToBitString :: Int -> Int -> BitString
numToBitString i s = BitString (str // nstr) s
  where nstr = zip [0..] $ map (== 1) $ numToBits i
        BitString {str} = makeBitString s

makeIndividual :: Int -> Int -> StdGen -> MultiBitString
makeIndividual bits params g = map (flip numToBitString bits) nums
  where nums = take params $ randomRs (0, 2 ^ bits - 1) g

makePopulation :: Int -> Int -> Int -> StdGen -> Population
makePopulation 0 _ _ _ = []
makePopulation popSize bits params g = ind : restPop
  where (g',g'') = split g
        restPop = makePopulation (popSize-1) bits params g''
        ind = makeIndividual bits params g'

flipBitAt :: Int -> Int -> MultiBitString -> MultiBitString
flipBitAt _ _ [] = []
flipBitAt b 0 (bs:bss) = newBs:bss
  where newBs = mutateBitString bs b
flipBitAt b p (bs:bss) = bs:(flipBitAt b (p-1) bss)

nthParam :: MultiBitString -> Int -> BitString
nthParam = (!!)

nthIndividual :: Population -> Int -> MultiBitString
nthIndividual = (!!)

nthIndividualParam :: Population -> Int -> Int -> BitString
nthIndividualParam p i j = (p `nthIndividual` i) `nthParam` j
