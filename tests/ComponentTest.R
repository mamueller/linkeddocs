#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
ComponentTest<-R6Class("ComponentTest",
	#inherit=InDirTest,
	inherit=ExamplePkgTest,
  public=list(
    targetPkgName=""
    ,
    #----------------
    evalWithExamplePackageLoaded=function(targetPkgName,expr){
      # copy the files 
      self$cp_package_files(targetPkgName)
      devtools::install('pkg')
      evalWithPackageLoaded(targetPkgName,expr)
    }
    ,
    #----------------
    test.exampleExtractionFromComments=function(){
        self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          m1=findMethods(exposedGeneric)[[1]]
        })
      )
    }
  )
)
############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  s$parallel <- 1 
  tr<-s$run()
  tr$summary()
}
