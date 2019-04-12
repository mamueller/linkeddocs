#!/bin/bash
set -ev
cd tests
#Rscript run_all_test.R
Rscript selected_travis.R
echo "Yeah"
