#!/bin/bash
set -ev
cd tests
#Rscript run_all_test.R
echo $(which Rscript)  
Rscript selected_travis.R
#echo $?
