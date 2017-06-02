#!/usr/bin/Rscript
## vim:set ff=unix expandtab ts=2 sw=2:
source("helpers.R")
source("ExamplePkgTest.R")
require(devtools,quiet=TRUE)
require("linkeddocs")
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
    test.correctNameSpaceInfo=function(){
      # To document a package properly we 
      # need to be sure that the genericFunctions and 
      # methods appear in the documentation as they will 
      # when the package is loaded.
      # To ensure this we load the package in 2 ways
      # The first one is to really use library and detach which is implemented 
      # in package.skeleton.dx_1
      # The second way is to create a new environment and source the 
      # relevant files in the package to be documented
      self$cp_package_files("ClassWithMethods")
      #self$cp_package_files("HiddenMehtod")
      pkgDir="pkg"
      nsi = package.skeleton.dx_1(pkgDir)
      #print(nsi)
      #alternative investigation based on devtools
      nsi_2 <- package.skeleton.dx_2(pkgDir)
      ref <- c("[","exposedGeneric")
      self$assertEqual(names(nsi$documentableMeths),ref)
      self$assertEqual(names(nsi_2$documentableMeths),ref)
      self$assertEqual(as.character(nsi_2$gens2),ref)

      ref <- c("exposedGeneric")
      self$assertEqual(as.character(nsi$GensWithSrc),ref)

      ref <- c("[","exposedGeneric","hiddenGeneric" )
      self$assertEqual(as.character(nsi_2$gens),ref)
      self$assertEqual(as.character(nsi_2$GensWithDocMethods),ref)

      ref <- c("exposedGeneric","hiddenGeneric" )
      self$assertEqual(as.character(nsi_2$GensWithSrc),ref)

	 	}
    ,
    #--------------------------------
    test.package.skeleton.dx_3=function(){
      pkgDir="pkg"
      self$cp_package_files("ClassWithMethods")
      nsi_2 <- package.skeleton.dx_3(pkgDir)
      pe(quote(list.files(file.path('pkg','man'))))
      cD <- 'check_dir'
      if (!dir.exists(cD)){
         dir.create(cD,recursive=TRUE)
       }
      l<-check(pkgDir,document=FALSE,quiet=TRUE,check_dir=cD)
      print(l)
    }
  )
)
############################################ 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
