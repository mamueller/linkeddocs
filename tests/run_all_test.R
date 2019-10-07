#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:
library(R6Unit)
#require(devtools,quiet=TRUE)
require("pkgload")
#require(linkeddocs)
pkgload::load_all('../pkg',export_all=FALSE)
s<-get_suitefromDiscoveredTestInstances(".",".*Test.*.R")
#s$parallel <- 1
print(s$test_names())
tr<-s$run()
tr$print_summary()
if( (tr$get_nFailures() + tr$get_nErrors())>0) stop(1)
