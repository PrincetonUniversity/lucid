#!/bin/bash
# Run the default tofino tests with compile / assemble / execute
cd ../../
make
cd -
unbuffer ./compilertests.py testspecs/tested_examples.json | tee "results/results.tested_examples.$(date +'%m-%d-%y').txt"
