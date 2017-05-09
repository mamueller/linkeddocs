#!/usr/bin/Rscript
## vim:set ff=unix expandtab ts=2 sw=2:
source("helpers.R")
source("ExamplePkgTest.R")
require(devtools,quiet=TRUE)
PrototypeTest<-R6Class("PrototypeTest",
	inherit=ExamplePkgTest,
	public=list(
    #--------------------------------
		test.src_refs=function(){
      self$targetPkgName<-"ClassWithMethods"
      prefix="../../../R"
      auto_paths=Sys.glob(paste(prefix,"*.R",sep="/"))
      for (f in auto_paths){
          print(f)
          #source(f,echo=FALSE)
      }
      document("pkg")
		}
  )
)
############################################ 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
