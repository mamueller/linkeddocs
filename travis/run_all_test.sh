#!/bin/bash
set -ev
cd ../tests/
Rscript run_all_test.R
echo "Yeah"
