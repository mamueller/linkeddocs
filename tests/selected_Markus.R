#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:

#source("R6_prolog.R")
require(R6Unit)
require(devtools,quiet=TRUE)
#devtools::install('../..')
require(linkeddocs)
#s<-get_suitefromDiscoveredTestInstances(".","^r6unit.*.R")
source("PackageTests_3.R")
source("PackageTests.R")
source("PrototypeTests.R")
#tr<-TestResults$new()
#tc<-PackageTests$new("test.NameSpaceParsing")
tc<-PackageTests$new("test.SoilR")
#tc<-PackageTests$new("test.abbriviatedSignature")
#tc<-PackageTests$new("test.GenericWithDotDotDotArgumets")
#tc <- PrototypeTests$new("test.package.skeleton.dx_3")
#tc<-PrototypeTests$new("test.correctNameSpaceInfo")
t1 <- PackageTests$new('test.ClassWithMethods')
t2 <- PackageTests_3$new('test.ClassWithMethods')
#t1<-PackageTests$new("test.OverloadedIndexOperator")
#s<-TestSuite$new(list(t1))
#s<-TestSuite$new(list(t2))
#s<-TestSuite$new(list(t2,t1)) # in this order the test work
s<-TestSuite$new(list(t1,t2)) # in this order the tests fail
print(s$test_names())
s$parallel <- 1 
tr <- s$run()
tr$summary()
