#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:

require(R6Unit)
require("pkgload")
pkgload::load_all('../pkg',export_all=FALSE)
#pkgload::load_all('~/debugHelpers/pkg',export_all=FALSE)
#pkgload::load_all('~/R6Unit/pkg',export_all=FALSE)
#source("ClassDocScriptTest.R")
source("PkgScriptTests.R")
source("ExampleExtractionTest.R")

s<-TestSuite$new(list(
  #PkgScriptTests$new("test.abbriviatedSignature")
  PkgScriptTests$new("test.abbriviatedSignatureMinimal")
  #PkgScriptTests$new("test.OverloadedIndexOperator")
  #PkgScriptTests$new("test.noMethodRdFilesForHiddenMethods")
  #PkgScriptTests$new("test.ClassWithMethods")
  #ExampleExtractionTest$new('test.methodExampleExtractionFromComments')
  #ExampleExtractionTest$new('test.extract_function_body_with_comments')
))
print(s$test_names())
#s$parallel <- 1 
tr <- s$run()
tr$print_summary()

