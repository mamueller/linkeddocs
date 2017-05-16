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
      require(linkeddocs)
    }
    ,
    #--------------------------------
    test.allMethods=function(){
      writeMethodRdFiles_fromSrcRef(env)
    }
    ,
    #--------------------------------
    test.selfload=function(){
      self$loadExamplePkg("linkeddocs")
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
      #self$loadExamplePkg("HiddenMehtod")
      pkgdir="pkg"
      nsi = mmNameSpaceInfo(pkgdir)
      #print(nsi)
      #alternative investigation based on devtools
      chdir <- file.path(pkgdir,"R")
      old.wd <- setwd(chdir)
      on.exit(setwd(old.wd))
      descfile <- file.path("..","DESCRIPTION")
      desc<-extract_description(descfile)
      pkgName <- as.character(read.dcf(descfile,fields=c('Package')))
      #print(pkgName)
      #exportedGenerics2 <- getGenerics(sprintf("package:%s",pkgName))
      #print('###########################')
      #print(exportedGenerics2)
      all <- devtools::load_all(export_all=FALSE)
      #fls <- roxygen2:::package_files('..')
      env <- all[['env']]
      print(name(env))
      #exportedGenerics <- getGenerics(where=env,searchForm=T)
      exportedGenerics <- getGenerics(sprintf("package:%s",pkgName))
      #names <- list()
      #for ( eg in exportedGenerics){
      #  o <- getGeneric(eg,env)
      #  v<-GenHasAnyMethodWithSrc(genName=eg,env=env)
      #  if (v){
      #    names <- append(eg,names)
      #    #print(names(o)) 
      #    #pp('v')
      #  }
      #}
      #pp('names')
      print(exportedGenerics)
      #self$assertEqual(exportedGenerics,nsi[['exportedGenerics']])
      #pp('eG',environment())
      #exG <- getGeneric('exposedGeneric',where=env)
      #codeDir<-utils::getSrcDirectory(exG)
      #codeFile<-utils::getSrcFilename(exG,full.names=T)
      #print(utils::getSrcLocation(exG))
      #print(class(utils::getSrcref(exG)))
      #print(length(utils::getSrcref(exG)))
      #print(codeFile)
      #print(lines)
      #print(findText(exG))
      # print(as.character(utils::getSrcref(exG),useSource=T))

		}
  )
)
############################################ 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
