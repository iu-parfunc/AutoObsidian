module Auto.Score
       (
         BitString
       , bitStringToNum
       , mutateBitString
       , crossBitString
       , numToBitString
       )
       where

import Data.Array
-- import Data.Array.Unboxed (UArray)

type ScoreType = Double 

class Score a where
  score :: a -> ScoreType

type BitString = Array Int Bool

-- Make a bitstring of size i.
makeBitString :: Int -> BitString
makeBitString i = array (0,i) [(j,False) | j <- [0..i]]

-- Convert bitstring to number.
bitStringToNum :: BitString -> Int
bitStringToNum arr = go $ assocs arr
  where go [] = 0
        go ((_,False):xs) = go xs
        go ((i,True):xs)  = (2^i) + (go xs)

-- Flip bit at i in bitstring.
mutateBitString :: BitString -> Int -> BitString
mutateBitString arr i = arr // [(i,b)]
  where b = not $ arr ! i

-- Crossover bitstrings arr1 and arr2.
crossBitString :: BitString -> BitString -> Int -> BitString
crossBitString arr1 arr2 i = array s l
  where s = bounds arr1
        l = zipWith f (assocs arr1) (assocs arr2)
        f (i1,v1) (_,v2)
          | (i1 < i)  = (i1,v1)
          | otherwise = (i1,v2)

-- Convert int to list of 0s and 1s.
numToBits :: Int -> [Int]
numToBits 0 = []
numToBits y = let (a,b) = quotRem y 2 in [b] ++ numToBits a

-- Make bitstring of number i and length s.
numToBitString :: Int -> Int -> BitString
numToBitString i s = makeBitString s // str
  where str = zip [0..] $ map (== 1) $ numToBits i
