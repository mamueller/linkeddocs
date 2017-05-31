#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
source("helpers.R")
source("PkgTest.R")
PackageTests<-R6Class("PackageTests",
	inherit=PkgTest,
	public=list(
    #--------------------------------
    test.SoilR=function(){
      self$checkExamplePkg("SoilR")
		}
    ,
    #--------------------------------
		test.abbriviatedSignature=function(){
      self$checkExamplePkg("Signatures")
		}
    ,
    #--------------------------------
    test.OverloadedIndexOperator=function(){
      self$checkExamplePkg("OverloadedIndexOperator")
		}
    ,
    #--------------------------------
    test.noMethodRdFilesForHiddenMethods=function(){
      self$checkExamplePkg("HiddenMethod")
		}
    ,
    #--------------------------------
    test.ClassWithMethods=function(){
      self$checkExamplePkg("ClassWithMethods")
		}
    ,
    #--------------------------------
    test.GenericWithDotDotDotArgumets=function(){
      self$checkExamplePkg("GenericWithDotDotDotArgumets")
		}
    ,
    #--------------------------------
    test.OverloadedIndexedAssingment=function(){
      self$checkExamplePkg("OverloadedIndexedAssingment")
		}
    #,
    ##--------------------------------
    #test.NameSpaceParsing=function(){
    #  self$checkExamplePkg("HiddenMethod")
    #  require("inlinedocs")
    #  inlinedocs:::pkgInfoFromInstall()

		#}
	)
)
#test.packageBuild=function(){
#	pkgDir="pkg"
#	RDir=file.path(pkgDir,"R")
#	TestDir=file.path(pkgDir,"inst","tests")
#	dir.create(RDir,recursive=TRUE)
#	#dir.create(TestDir,recursive=TRUE)
# srcCode='
##------------------------------------------------
##------------------------------------------------
## define Generic Funtions
##------------------------------------------------
##------------------------------------------------
#setGeneric(
#    name="hiddenGeneric",
#    def=function # extract times
#    ### The method extracts the time from the object
#    (  
#    object ##<< an object 
#    ){
#        standardGeneric("hiddenGeneric")
#    }
#)
##------------------------------------------------
#setGeneric(
#    name="exposedGeneric",
#    def=function( # extract times from various kinds of obhects
#    ### The function extract the time property of an object of varios classes if the class implements a mehtod for it
#    object ##<< an object 
#    ){
#        standardGeneric("exposedGeneric")
#    }
#)
##------------------------------------------------
## define classes with mehtods
##------------------------------------------------
#setClass(# HiddenClass
#   Class="HiddenClass",
#   representation=representation(
#        times="numeric"
#   )
#)
##------------------------------------------------
#setMethod(
#   f= "hiddenGeneric",
#   signature="HiddenClass",
#   definition=function # extract times
#   ### The method extracts the time from the object
#   (
#   object ##<< an object
#   ){
#       return(object@times)
#     }
#)
#
##------------------------------------------------
## the next method should not appear in the help file exposedGeneric-methods.Rd 
## because this class is hidden although the generic is exposed.
## so only methods for exposed classes should show up in the file
#setMethod(
#   f= "exposedGeneric",
#   signature="HiddenClass",
#   definition=function # extract times
#   ### The method extracts the time from the object
#   (
#   object ##<< an object
#   ){
#       return(object@times)
#     }
#)
##------------------------------------------------
## overload the [[ operator which is done only here.
## but since the class is hidden the cooresponding Method desription file z-[[-methods would be empty
## so preferably it should disappear completely
## (template created by: method.skeleton("[[","HiddenClass")
#setMethod("[[",
#    signature(x = "HiddenClass"),
#    function # [[]] for Hidden Class
#    ### this method implements the [[]] for objects of "HiddenClass"
#    (x, i, j, ...) 
#    {
#        print("I am a hidden method because I belong to a class which is not exported in the NAMESPACE File")
#    }
#)
#setClass(#ExposedClass
#   Class="ExposedClass",
#   representation=representation(
#        times="numeric"
#   )
#)
##------------------------------------------------
## the next method should not appear in the help 
## because the generic function is not exported
#setMethod(
#   f= "hiddenGeneric",
#   signature="ExposedClass",
#   definition=function# extract times  
#   ### The method extracts the time from the object
#   (
#       object ##<< an object of class ExposedClass
#    ){
#       return(object@times)
#     }
#)
##------------------------------------------------
#setMethod(
#   f= "exposedGeneric",
#   signature="ExposedClass",
#   definition=function(# extract times
#   ### The method extracts the time from the object
#                       
#       object ##<< an object of class ExposedClass
#       ){
#       return(object@times)
#     }
#)
##------------------------------------------------
## overload the [] operator
#  #define a hidden helper function (not in the NAMESPACE file   
#  getSingleCol=function# a helper
#  ### a helper that will not appear in the documentation directly
#  (
#                        x,slot_name){
#      res=""
#      if(slot_name=="times"){ res=exposedGeneric(x)}
#      return(res)
#  }
#  #define the actual method
#  setMethod("[",signature(x="ExposedClass",i="character"), #since [] is a already defined generic the names of the arguments are not arbitrary 
#  definition=function #"[]" for "Exposed Class"
#  ### this method implements the [] for objects of "Exposed Class"
#  (x,i){
#      n=length(i)
#      df=getSingleCol(x,i[1])
#      if (n>1){
#          for (k in 2:n){
#              df=cbind(df,getSingleCol(x,i[k]))
#          }
#      }
#      return(df)
#  }
#)
##------------------------------------------------
## overload the $ operator
#setMethod("$",signature(x="ExposedClass"), #since $ is a already defined generic the names of the arguments are not arbitrary 
#        definition=function #overload $ for ExposedClass objects
#        ### This method implements $ for objects of ExposedClass
#        (x,name){
#            return(getSingleCol(x,name))
#        }
#)
#
##------------------------------------------------
##------------------------------------------------
## define simple functions
##------------------------------------------------
##------------------------------------------------
#hiddenFunction=function# hidden square
####  The function squares its argument
#(
# x ##<< a number
#){
#   return(x^2)
#}
##------------------------------------------------
#exposedFunction=function #a function that will have some description created by inlinedocs
#### inlinedocs will create a Rd file for this function since it is exported in the namespace file and can be called by the user 
#### of the package (who installed it)
#(x  ##<< a number
# ){
#   result=hiddenFunction(x)
#   return(result)
#   ### Another number whose computation is top secret because it is carried out by a function not exposed in the namespace file
#}
#'
#  f=file.path(RDir,"source.R")
#	cat(file=f,srcCode)
#pkgName='NamespaceExample'  
#pkgVersion='0.0-1'  
#desc <-paste("
#Package:",pkgName," 
#Title: Examples to test the possibilities of Namespaces  
#Version:",pkgVersion,"
#Date: 2013-03-4
#Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
#Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
#Description: This package contains some functions to be tested
#License: GPL-3
#Depends:methods,RUnit 
#",sep="")
#
#  descFilePath=file.path(pkgDir,"DESCRIPTION")
#	cat(file=descFilePath,text=desc)
#namesp <- '
#exportClasses(
#ExposedClass
#)
#'
#  NamespaceFilePath=file.path(pkgDir,"NAMESPACE")
#	cat(file=NamespaceFilePath,text=namesp)
#	parsers=NULL
#  #result <- extract.docs.file(f,parsers) #[["exposedFunction"]]
#	#pp("result",environment())
#  #package.skeleton.dx(pkgDir)
#  tryCatch(package.skeleton.dx(pkgDir))
#  res=system(paste("R CMD build",pkgDir))
# 	checkEquals(res,0,"error in R CMD build")
#  res=system(paste("R CMD check",pkgDir))
# 	checkEquals(res,0,"error in R CMD check ")
#  cf=paste(pkgDir,".Rcheck/00check.log",sep="")
#  cfl=readLines(cf)
#  checkTrue(!any(grepl("warning",cfl,ignore.case = TRUE)))
#
#}
#
##------------------------------------------------#####################
#test.changedMethodsRdFilesForHiddenGeneric=function(){
#  # A special case of an exported generic is an overloaded operator:
#  # If the operator is allready defined by R base or one of the 
#  # imported packages it does >> not << have to be explicitly mentioned 
#  # in an export part of the NAMESPACE file.
#  # This implies that a -methods.Rd file will be created
#  # e.g. "z[[-methods.Rd" for the "[[" operator 
#  # although this operator is >> not << mentioned in the 
#  # NAMESPACE as exported e.g. because it is only implemented by a 
#  # hidden class.
#  # In this case the method section of "z[[-methods.Rd" would be empty
#  # causing a NOTE on cran checks.
#  # Actually it seems reasonable to remove "z[[-methods.Rd"  completely in this case
#  # but unfortunately the cran checks insist on it beeing present.
#
#  # We want to make sure that the file is different for Methods with Sinatures 
#  # containing Hidden Classes  
#  # For our example the "[[" operator  is only overloaded in HiddenClass.R
#  # so the file z[[-methods.Rd should be altered to have no method section.
#
#	pkgDir="pkg"
#	RDir=file.path(pkgDir,"R")
#	TestDir=file.path(pkgDir,"inst","tests")
#	dir.create(RDir,recursive=TRUE)
#	#dir.create(TestDir,recursive=TRUE)
# srcCode='
##------------------------------------------------
##------------------------------------------------
## define classes with mehtods
##------------------------------------------------
##------------------------------------------------
#setClass(# HiddenClass
#   Class="HiddenClass",
#   representation=representation(
#        times="numeric"
#   )
#)
##------------------------------------------------
#setClass(#ExposedClass
#   Class="ExposedClass",
#   representation=representation(
#        times="numeric"
#   )
#)
##------------------------------------------------
## overload the [[ operator which is done only for the HiddenClass 
## but since the class is hidden the cooresponding Method desription file z-[[-methods would be empty
## so preferably it should disappear completely
## (template created by: method.skeleton("[[","HiddenClass")
#setMethod("[[",
#    signature(x = "HiddenClass"),
#    function # [[]] for Hidden Class
#    ### this method implements the [[]] for objects of "HiddenClass"
#    (x, i, j, ...) 
#    {
#        print("I am a hidden method because I belong to a class which is not exported in the NAMESPACE File")
#    }
#)
#'
#  f=file.path(RDir,"source.R")
#	cat(file=f,srcCode)
#pkgName='NamespaceExample'  
#pkgVersion='0.0-1'  
#desc <-paste("
#Package:",pkgName," 
#Title: Examples to test the possibilities of Namespaces  
#Version:",pkgVersion,"
#Date: 2013-03-4
#Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
#Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
#Description: This package contains some functions to be tested
#License: GPL-3
#Depends:methods,RUnit 
#",sep="")
#
#  descFilePath=file.path(pkgDir,"DESCRIPTION")
#	cat(file=descFilePath,text=desc)
#namesp <- '
#exportClasses(
#ExposedClass
#)
#'
#  NamespaceFilePath=file.path(pkgDir,"NAMESPACE")
#	cat(file=NamespaceFilePath,text=namesp)
#  package.skeleton.dx(pkgDir,inlinedocs.documentNamespaceOnly=TRUE)
#  manDir <- file.path(pkgDir,"man")
#  zpath <- file.path(manDir,"z[[-methods.Rd") 
#  checkTrue(file.exists(zpath))
#  strA <- '
#\\name{[[-methods}
#\\docType{methods}
#\\alias{[[-methods}
#\\alias{[[,HiddenClass-method}
#\\title{ ~~ Methods for Function \\code{[[}  ~~}
#\\description{
# All methods for function \\code{[[} are intended for internal use inside the package only. \n}
#\\keyword{methods}
#\\keyword{ ~~ other possible keyword(s) ~~ }
#  '
#  if(!CompareTrimmedNonEmptyLines(readLines(zpath),strA)){
#    refZpath=paste(zpath,".Ref",sep="")
#    unlink(refZpath)
#    cat(file=refZpath,strA)
#    #system(paste("gvimdiff",refZpath,zpath,"&"))
#    stop()
#  }
#  res=system(paste("R CMD build",pkgDir))
# 	checkEquals(res,0,"error in R CMD build")
#  command=paste("R CMD check --as-cran",pkgDir)
#  res=system(command)
# 	checkEquals(res,0,paste("error in",command))
#  cf=paste(pkgDir,".Rcheck/00check.log",sep="")
#  cfl=readLines(cf)
#  checkTrue(!any(grepl("warning",cfl,ignore.case = TRUE)))
#}
#
#
#
##------------------------------------------------#####################
#test.noMethodRdFilesForHiddenMethods<-function(){
#    testSrcDir<-"../.."
#    source(file.path(testSrcDir,"createNameSpacePackage.R"))
#    pkgDir<-"pkg"
#    createNameSpaceExample(pkgDir)
#
#  package.skeleton.dx(pkgDir,inlinedocs.documentNamespaceOnly=FALSE)
#}
##  manDir <- file.path(pkgDir,"man")
##  allFileNames <-  c(
##    "ExposedClass-class.Rd",
##    "HiddenClass-class.Rd",
##    "NamespaceExample-package.Rd",
##    "exposedFunction.Rd",
##    "exposedGeneric-methods.Rd",
##    "exposedGeneric.Rd",
##    "exposedGeneric,ExposedClass-method.Rd",
##    "exposedGeneric,ExposedHiddenClass-method.Rd",
##    "getSingleCol.Rd",
##    "hiddenFunction.Rd",
##    "hiddenGeneric-methods.Rd",
##    "hiddenGeneric.Rd",
##    "hiddenGeneric_method__ExposedClass.Rd",
##    "hiddenGeneric_method__HiddenClass.Rd",
##    "z$-methods.Rd",
##    "z$_method__ExposedClass.Rd",
##    "z[-methods.Rd",
##    "z[[-methods.Rd",
##    "z[[_method__HiddenClass.Rd",
##    "z[_method__ExposedClass_character.Rd"
##  )
###  res=list.files(path=manDir,pattern=".*\\.Rd")
###  #pp("res",environment())
### 	checkEquals(res,allFileNames)
### 
###  
###  
###  unlink(manDir,recursive=TRUE)
###  dir.create(manDir,recursive=TRUE)
##  package.skeleton.dx(pkgDir,inlinedocs.documentNamespaceOnly=TRUE)
##  # the following files should be missing if we document only objects exported 
##  # in the NAMESPACE file 
##  filesExpectedMissing <- c(
##    "HiddenClass-class.Rd",                 #because the class is not exported
##    #"exposedGeneric_method__HiddenClass.Rd",#because the class in the signature is not exported
##    "getSingleCol.Rd",                        # function not exported
##    "hiddenFunction.Rd",                    # function not exported
##    "hiddenGeneric-methods.Rd",             # generic not exported
##    "hiddenGeneric.Rd",                     # generic not exported
##    "hiddenGeneric_method__ExposedClass.Rd",# generic not exported
##    "hiddenGeneric_method__HiddenClass.Rd"  # generic not exported
##  )
##  remainingFileNames <-setdiff( allFileNames,filesExpectedMissing)
##  res=list.files(path=manDir,pattern=".*\\.Rd")
## 	checkEquals(sort(res),sort(remainingFileNames))
##  command=paste("R CMD check --as-cran",pkgDir)
##  res=system(command)
## 	checkEquals(res,0,paste("error in",command))
##  cf=paste(pkgDir,".Rcheck/00check.log",sep="")
##  cfl=readLines(cf)
##  checkTrue(!any(grepl("warning",cfl,ignore.case = TRUE)))
##}
##------------------------------------------------#####################
#test.manualDocExample=function(){
#  # The test documents weird behaviour of check --as-cran
#  # which complains about a missing Rd file for an operator that was
#  # only overloaded for a hidden class.
#
#  # A special case of an exported generic is an overloaded operator:
#  # If the operator is allready defined by R base or one of the 
#  # imported packages it does >> not << have to be explicitly mentioned 
#  # in an export part of the NAMESPACE file.
#  # This implies that a -methods.Rd file will be created
#  # e.g. "z[[-methods.Rd" for the "[[" operator 
#  # although this operator is >> not << mentioned in the 
#  # NAMESPACE as exported e.g. because it is only implemented by a 
#  # hidden class.
#  # In this case the method section of "z[[-methods.Rd" should be empty
#  # Actually it seems reasonable to remove "z[[-methods.Rd"  completely in this case
#  # but unfortunately the cran checks insist on it beeing present.
#  # and also cause a NOTE if the method section is empty.
#  # The purpose of this test is to manually construct a "z[[-methods.Rd"
#  # thad passes the cran checks of the package
#  
#	pkgDir="pkg"
#	RDir=file.path(pkgDir,"R")
#	dir.create(RDir,recursive=TRUE)
# srcCode='
##------------------------------------------------
##------------------------------------------------
## define classes with mehtods
##------------------------------------------------
##------------------------------------------------
#setClass(# HiddenClass
#   Class="HiddenClass",
#   representation=representation(
#        times="numeric"
#   )
#)
##------------------------------------------------
#setClass(#ExposedClass
#   Class="ExposedClass",
#   representation=representation(
#        times="numeric"
#   )
#)
##------------------------------------------------
## overload the [[ operator which is done only for the HiddenClass 
## but since the class is hidden the cooresponding Method desription file z-[[-methods would be empty
## so preferably it should disappear completely
## (template created by: method.skeleton("[[","HiddenClass")
#setMethod("[[",
#    signature(x = "HiddenClass"),
#    function # [[]] for Hidden Class
#    ### this method implements the [[]] for objects of "HiddenClass"
#    (x, i, j, ...) 
#    {
#        print("I am a hidden method because I belong to a class which is not exported in the NAMESPACE File")
#    }
#)
#'
#  f=file.path(RDir,"source.R")
#	cat(file=f,srcCode)
#pkgName='NamespaceExample'  
#pkgVersion='0.0-1'  
#desc <-paste("
#Package:",pkgName," 
#Title: Examples to test the possibilities of Namespaces  
#Version:",pkgVersion,"
#Date: 2013-03-4
#Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
#Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
#Description: This package contains some functions to be tested
#License: GPL-3
#Depends:methods,RUnit 
#",sep="")
#
#  descFilePath=file.path(pkgDir,"DESCRIPTION")
#	cat(file=descFilePath,text=desc)
#namesp <- '
#exportClasses(
#ExposedClass
#)
#'
#  NamespaceFilePath=file.path(pkgDir,"NAMESPACE")
#	cat(file=NamespaceFilePath,text=namesp)
#
#  package.skeleton.dx(pkgDir,inlinedocs.documentNamespaceOnly=TRUE)
#  manDir <- file.path(pkgDir,"man")
#  # overwrite the Rd file manually
#  zpath <- file.path(manDir,"z[[-methods.Rd") 
#  strA <- '
#\\name{[[-methods}
#\\docType{methods}
#\\alias{[[-methods}
#\\alias{[[,HiddenClass-method}
#        
#\\title{ ~~ Methods for Function \\code{[[}  ~~}
#\\description{
# ~~ Methods for function \\code{[[}  ~~
#}
#\\keyword{methods}
#\\keyword{ ~~ other possible keyword(s) ~~ }
#  '
#  cat(file=zpath,strA)
#  res=system(paste("R CMD build",pkgDir))
# 	checkEquals(res,0,"error in R CMD build")
#  command=paste("R CMD check --as-cran",pkgDir)
#  res=system(command)
# 	checkEquals(res,0,paste("error in",command))
#  cf=paste(pkgDir,".Rcheck/00check.log",sep="")
#  cfl=readLines(cf)
#  checkTrue(!any(grepl("warning",cfl,ignore.case = TRUE)))
#}


############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  s$parallel <- 1 
  tr<-s$run()
  tr$summary()
}
