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
    setUp=function(){
      #prefix="../../../../R"
      #auto_paths=Sys.glob(paste(prefix,"*.R",sep="/"))
      #for (f in auto_paths){
      #    source(f,echo=FALSE)
      #}
    }
    ,
    #--------------------------------
    test.correctNameSpaceInfo=function(){
      # To document a package properly we 
      # need to be sure that the genericFunctions and 
      # methods appear in the documentation as they will 
      # when the package is loaded.
      # 
      self$loadExamplePkg("ClassWithMethods")
      #prefix="../../../../R"
      #auto_paths=Sys.glob(paste(prefix,"*.R",sep="/"))
      #for (f in auto_paths){
      #    source(f,echo=FALSE)
      #}

      pkgdir="pkg"
      #print(mmNameSpaceInfo(pkgdir))
      #alternative investigation
      chdir <- file.path(pkgdir,"R")
      old.wd <- setwd(chdir)
      on.exit(setwd(old.wd))
      descfile <- file.path("..","DESCRIPTION")
      
      desc<-extract_description(descfile)
      #print(desc) 
      all <- devtools::load_all()
      fls <- roxygen2:::package_files('..')
      print(all)
      env <- all[['env']]
      pkgName<-packageDescription(pkgdir,".",fields="Package")  
      print(pkgName)
      eG <- getGenerics(where=env)
      exG <- getGeneric('exposedGeneric',where=env)
      print('###########################')
      #codeDir<-utils::getSrcDirectory(exG)
      #codeFile<-utils::getSrcFilename(exG,full.names=T)
      #print(utils::getSrcLocation(exG))
      #print(class(utils::getSrcref(exG)))
      #print(length(utils::getSrcref(exG)))
      #print(codeFile)
      #print(lines)
      print(findText(exG))
      # print(as.character(utils::getSrcref(exG),useSource=T))
      writeMethodRdFiles_fromSrcRef(env)

		}
    ,
    #--------------------------------
    test.consistentS4Naming=function(){
      print('blub')
    }
  )
)
############################################ 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
