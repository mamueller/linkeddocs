#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:
source("helpers.R")
library(devtools)
s<-get_suitefromDiscoveredTestInstances(".",".*Test.*.R")
s$parallel <- 1
print(s$test_names())
tr<-s$run()
tr$summary()
