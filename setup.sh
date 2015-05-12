#!/bin/bash 


cabal sandbox init 

cabal install ./Obsidian/ . ./Applications/Graph/ ./Applications/Mandel/  ./Applications/Histogram ./Applications/Reduction/ ./Applications/ReductionSeq/ 

