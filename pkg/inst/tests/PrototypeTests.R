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
    test.consistentS4Naming=function(){
      print('blub')
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
      pkgdir="pkg"
      #nsi <- mmNameSpaceInfo(pkgdir)
      #alternative investigation based on devtools
      chdir <- file.path(pkgdir,"R")
      old.wd <- setwd(chdir)
      on.exit(setwd(old.wd))
      descfile <- file.path("..","DESCRIPTION")
      desc<-extract_description(descfile)
      pkgName <- as.character(read.dcf(descfile,fields=c('Package')))
      print(pkgName)
      exportedGenerics2 <- getGenerics(sprintf("package:%s",pkgName))
      print('###########################')
      print(exportedGenerics2)
      #all <- devtools::load_all()
      #fls <- roxygen2:::package_files('..')
      #env <- all[['env']]
      #print(env)
      #exportedGenerics <- getGenerics(where=env)
      #names <- list()
      #for ( eg in exportedGenerics){
      #  names <- append(eg,names)
      #  o <- getGeneric(eg,env)
      #  #print(o)
      #  print(GenHasAnyMethodWithSrc(genName=eg,env=env))
      #}
      #print(names)
      #print(nsi[['exportedGenerics']])
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
      #writeMethodRdFiles_fromSrcRef(env)

		}
  )
)
############################################ 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
