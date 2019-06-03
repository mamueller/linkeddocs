#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:
library(R6Unit)
require(devtools,quiet=TRUE)
require(linkeddocs)
source("helpers.R")
s<-get_suitefromDiscoveredTestInstances(".",".*Test.*.R")
#s$parallel <- 1 does not work for some calls to devtools (problems with a file connection)
print(s$test_names())
tr<-s$run()
tr$summary()
if( (tr$get_nFailures() + tr$get_nErrors())>0) stop(1)
