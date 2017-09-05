#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
ComponentsTest<-R6Class("ComponentsTest",
	inherit=ExamplePkgTest,
  public=list(
    #----------------
    evalWithExamplePackageLoaded=function(targetPkgName,expr){
      # copy the files 
      self$cp_package_files(targetPkgName)
      tryTwice(quote(devtools::install('pkg',keep_source=TRUE)))
      evalWithPackageLoaded(targetPkgName,expr)
    }
  )
)
