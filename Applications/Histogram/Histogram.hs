

module Histogram where

import Obsidian

import Prelude



-- histogram1D :: EW32
--             -> Mutable Global EW32 EW32
--             -> DPull EW32
--             -> GProgram () 
-- histogram1D width mutable input =
--   forAll width $ \i ->
--     atomicInc (input ! i) mutable


histogram :: EW32 -- Width of image
            -> EW32 -- Height of image 
            -> Mutable Global EW32 EW32
            -> DPull EW32
            -> GProgram ()
histogram width height mutable input =
  distrPar height $ \i -> 
    forAll width $ \j ->
      atomicInc (input ! (i * width + j)) mutable 

             

