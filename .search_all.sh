#!/bin/bash 


# Perform all searches 

# expect executables in ./.cabal-sandbox/bin/

EXECDIR=./.cabal-sandbox/bin
CSVROOT=/u/crest-team/Joel/AUTOCSV

# EXHAUSTIVE RANDOM'
STRATEGIES='BITCLIMB SGA' 

#create a directory labeled with start time of experiment
TARGETDIR=$CSVROOT/search_$(date +"%d%m%y_%H%M%S")
mkdir $TARGETDIR

for strat in $STRATEGIES; do 

    echo "***Starting $strat search strategy***"
    
    echo "***SEARCH MANDELBROT***"
    $EXECDIR/Mandel2 $strat THREADS
    $EXECDIR/Mandel2 $strat BOTH 

    echo "***SEARCH HISTOGRAM***"
    $EXECDIR/Histogram $strat THREADS 
    $EXECDIR/Histogram $strat BOTH     #threads & blocks 
    
    echo "***SEARCH REDUCTION***"
    $EXECDIR/Reduction $strat WARPTH
    $EXECDIR/Reduction $strat BOTH 

    echo "***SEARCH REDUCTION SEQ***" 
    $EXECDIR/Reduction $strat SEQTH
    $EXECDIR/Reduction $strat BOTH 

    echo "***SEARCH GRAPH BFS***"
    #perform Graph 
    $EXECDIR/Graph $strat 1 # Just KERNEL_TH  
    $EXECDIR/Graph $strat 2 # KERNEL_TH and SMALL_VERTEX 
    
    # move CSVs to CSV storage 
    mv *.csv $TARGETDIR
    
done; 




