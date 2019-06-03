#!/bin/bash
set -ev
libdir=$(pwd)/mylib
mkdir -p $libdir
export R_LIBS_USER=$libdir
Rscript travis/inst.R
cd tests
Rscript run_all_test.R
#echo $(which Rscript)  
#Rscript selected_travis.R
echo $?
