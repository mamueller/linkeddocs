#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:

#source("R6_prolog.R")
source("helpers.R")
require(R6Unit)
require(inlinedocs)
#s<-get_suitefromDiscoveredTestInstances(".","^r6unit.*.R")
source("r6unit.PackageTests.R")
tr<-TestResults$new()
tc<-PackageTests$new("test.NameSpaceParsing")
#tc<-PackageTests$new("test.SoilR")
#tc<-PackageTests$new("test.abbriviatedSignature")
#tc<-PackageTests$new("test.GenericWithDotDotDotArgumets")
#tc<-PackageTests$new("test.OverloadedIndexedAssingment")

tc$run(tr)
#stop(mmsg())

tr$summary()
