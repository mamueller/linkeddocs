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
    test.selfload=function(){
      # demonstrate how a package with the same name and function names similar to 
      # linkeddocs functions can be loaded
      # without interfering
      self$loadExamplePkg("linkeddocs")
      require(methods)
      myEnv <- new.env(parent=globalenv())
      myPkgName <- "linkedoc_devtest"
      methods::setPackageName(myPkgName, myEnv)
      pkg<-as.package('pkg')
      paths<- devtools:::find_code(pkg)
      devtools:::withr_with_dir(file.path(pkg$path), devtools:::source_many(paths, myEnv))
      tf <- mmNameSpaceInfo
      d1 <- findText(tf)
      # compare the first line of code
      # first the original one
      self$assertEqual(d1[1],"function(pkgDir){")

      tf2 <- myEnv[['mmNameSpaceInfo']]
      d2 <- findText(tf2)
      # now the new function of the package to be documented
      self$assertEqual(d2[1],"function( # a fake only there to test if we can avoid overloading the original function in linkeddocs")
    }
    #,
    ##--------------------------------
    #test.allMethods=function(){
    #  writeMethodRdFiles_fromSrcRef(env)
    #}
    ,
    #--------------------------------
    test.correctNameSpaceInfo=function(){
      # To document a package properly we 
      # need to be sure that the genericFunctions and 
      # methods appear in the documentation as they will 
      # when the package is loaded.
      # To ensure this we load the package in 2 ways
      # The first one is to really use library and detach which is implemented 
      # in mmNameSpaceInfo
      # The second way is to create a new environment and source the 
      # relevant files in the package to be documented
      self$loadExamplePkg("ClassWithMethods")
      #self$loadExamplePkg("HiddenMehtod")
      pkgDir="pkg"
      nsi = mmNameSpaceInfo(pkgDir)
      #print(nsi)
      #alternative investigation based on devtools
      descfile <- file.path(pkgDir,"DESCRIPTION")
      print(descfile)
      desc<-extract_description(descfile)
      pkgName <- as.character(read.dcf(descfile,fields=c('Package')))
      print(pkgName)
      myEnv <- new.env(parent=globalenv())
      myPkgName <- "linkedoc_devtest"
      methods::setPackageName(myPkgName, myEnv)
      pkg<-as.package('pkg')
      paths<- devtools:::find_code(pkg)
      devtools:::withr_with_dir(file.path(pkg$path), devtools:::source_many(paths, myEnv))
       allGenerics <- getGenerics(where=myEnv,searchForm=T)
       print(allGenerics)
       #exportedGenerics <- getGenerics(sprintf("package:%s",pkgName))
       ##names <- list()
       ##for ( eg in exportedGenerics){
       ##  o <- getGeneric(eg,env)
       ##  v<-GenHasAnyMethodWithSrc(genName=eg,env=env)
       ##  if (v){
       ##    names <- append(eg,names)
       ##    #print(names(o)) 
       ##    #pp('v')
       ##  }
       ##}
       ##pp('names')
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
