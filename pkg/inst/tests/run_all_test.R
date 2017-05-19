#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:
#source("R6_prolog.R")
source("helpers.R")
library(devtools)
devtools::install("../../")
s<-get_suitefromDiscoveredTestInstances(".",".*Test.*.R")
print(s$test_names())
tr<-s$run()
tr$summary()
