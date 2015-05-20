#!/bin/bash 


# Perform all searches 

# expect executables in ./.cabal-sandbox/bin/

EXECDIR=./.cabal-sandbox/bin
CSVROOT=/u/crest-team/Joel/AUTOCSV

# EXHAUSTIVE RANDOM'
STRATEGIES=$1 

APPLICATIONS=$2 

echo $STRATEGIES
#create a directory labeled with start time of experiment
TARGETDIR=$CSVROOT/search_$(date +"%d%m%y_%H%M%S")
mkdir $TARGETDIR

for strat in $STRATEGIES; do 

    for app in $APPLICATIONS; do 
    
	echo "***Starting $strat search strategy***"
    
	case $app in 
	    MANDEL)
		echo "***SEARCH MANDELBROT***"
		$EXECDIR/Mandel2 $strat THREADS
		$EXECDIR/Mandel2 $strat BOTH 
		;;
	    HISTO) 
		echo "***SEARCH HISTOGRAM***"
		$EXECDIR/Histogram $strat THREADS 
		$EXECDIR/Histogram $strat BOTH     #threads & blocks 
		;;
	    REDUCE) 
		echo "***SEARCH REDUCTION***"
		$EXECDIR/Reduction $strat WARPTH
		$EXECDIR/Reduction $strat BOTH 
		$EXECDIR/Reduction $strat WTB     # warp_th, threads, blocks  
		;;
	    REDUCESEQ)
		echo "***SEARCH REDUCTION SEQ***" 
		$EXECDIR/ReductionSeq $strat SEQTH
		$EXECDIR/ReductionSeq $strat BOTH 
		$EXECDIR/ReductionSeq $strat STB  #seq_th, threads, blocks 
		;;
	    GRAPH)
		echo "***SEARCH GRAPH BFS***"
		#perform Graph 
		$EXECDIR/Graph $strat 1 # Just KERNEL_TH  
		$EXECDIR/Graph $strat 2 # KERNEL_TH and SMALL_VERTEX 
		$EXECDIR/Graph $strat LIMITED # same as 2 but with smaller range
		;;
	esac
	    
    done; 
    
    # move CSVs to CSV storage 
    mv *.csv $TARGETDIR
    
done; 





