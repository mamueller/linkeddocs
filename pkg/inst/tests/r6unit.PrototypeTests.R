#!/usr/bin/Rscript
## vim:set ff=unix expandtab ts=2 sw=2:
source("helpers.R")
source("ExamplePkgTest.R")
require(devtools,quiet=TRUE)
PrototypeTests<-R6Class("PrototypeTests",
	inherit=ExamplePkgTest,
	#inherit=InDirTest,
	public=list(
    #--------------------------------
    test.consistentS4Naming=function(){
      # To document a package properly we 
      # need to be sure that the genericFunctions and 
      # methods appear in the documentation as they will 
      # when the package is loaded.
      # 
      self$loadExamplePkg("ClassWithMethods")
      prefix="../../../../R"
      auto_paths=Sys.glob(paste(prefix,"*.R",sep="/"))
      for (f in auto_paths){
          source(f,echo=FALSE)
      }

      pkgdir="pkg"
      print(mmNameSpaceInfo(pkgdir))
      #alternative investigation
      chdir <- file.path(pkgdir,"R")
      old.wd <- setwd(chdir)
      on.exit(setwd(old.wd))
      print(getwd())
      descfile <- file.path("..","DESCRIPTION")
      
      desc<-extract_description(descfile)
      print(desc) 
		}
  )
)
############################################ 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
