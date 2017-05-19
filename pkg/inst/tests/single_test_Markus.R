#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:

#source("R6_prolog.R")
require(R6Unit)
#require(linkeddocs)
#s<-get_suitefromDiscoveredTestInstances(".","^r6unit.*.R")
source("PackageTests.R")
source("PrototypeTests.R")
tr<-TestResults$new()
#tc<-PrototypeTests$new("test.consistentS4Naming")
#tc<-PackageTests$new("test.NameSpaceParsing")
#tc<-PackageTests$new("test.SoilR")
#tc<-PackageTests$new("test.abbriviatedSignature")
#tc<-PackageTests$new("test.GenericWithDotDotDotArgumets")
tc<-PackageTests$new("test.OverloadedIndexOperator")
#tc <- PrototypeTests$new("test.correctNameSpaceInfo")

tc$run(tr)
#stop(mmsg())

tr$summary()
