#!/bin/bash

set -xe

if [ "$CABAL" == "" ]; then
    CABAL=cabal
fi


(cd Obsidian; git checkout master-dev) 

$CABAL sandbox init

OPTS="-j --ghc-option=-j3"
$CABAL install $OPTS --run-tests ./Obsidian/ ./src/ ./Applications/Mandel ./Applications/Histogram

# Test suite?  Benchmark Suite?
# Currently this does not share code with .travis.yml, and the Travis version does more.
