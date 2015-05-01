#!/bin/bash

set -xe

if [ "$CABAL" == "" ]; then
    CABAL=cabal
fi

# Manually update just ONE submodule.  Not testing the gpu_graph one currently:
git submodule update --init Obsidian
# This is a no-no, it means different things at different times and is
# thus not reproducable:
# (cd Obsidian; git checkout master-dev)

$CABAL sandbox init

OPTS="-j --ghc-option=-j3"
$CABAL install $OPTS --run-tests . ./Obsidian/ ./Applications/Mandel ./Applications/Histogram

# Test suite?  Benchmark Suite?
# Currently this does not share code with .travis.yml, and the Travis version does more.
