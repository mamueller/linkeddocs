#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
ComponentTest<-R6Class("ComponentTest",
	#inherit=InDirTest,
	inherit=ExamplePkgTest,
  public=list(
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
        print(.libPaths())
        self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          print('Markus')
          meths <- findMethods(exposedGeneric)
          sig=meths[[1]]@defined
          print(sig)
        })
      )
    }
  )
)
############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  source("helpers.R")
  s=get_suite_from_file(get_Rscript_filename())
  s$parallel <- 1 
  tr<-s$run()
  tr$summary()
}
