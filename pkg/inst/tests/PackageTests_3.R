#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
require(devtools)
devtools::install('../..')
source("helpers.R")
source("PkgTest_3.R")
PackageTests_3<-R6Class("PackageTests_3",
	inherit=PkgTest_3,
	public=list(
    #--------------------------------
    test.SoilR=function(){
      self$checkExamplePkg("SoilR")
	  }
    ,
  #  #--------------------------------
	#  test.abbriviatedSignature=function(){
  #    self$checkExamplePkg("Signatures")
	#  }
  #  ,
  #  #--------------------------------
  #  test.OverloadedIndexOperator=function(){
  #    self$checkExamplePkg("OverloadedIndexOperator")
	#  }
  #  ,
  #  #--------------------------------
  #  test.noMethodRdFilesForHiddenMethods=function(){
  #    self$checkExamplePkg("HiddenMethod")
	#  }
  #  ,
  #  #--------------------------------
  #  test.GenericWithDotDotDotArgumets=function(){
  #    self$checkExamplePkg("GenericWithDotDotDotArgumets")
	#  }
  #  ,
  #  #--------------------------------
  #  test.OverloadedIndexedAssingment=function(){
  #    self$checkExamplePkg("OverloadedIndexedAssingment")
	#  }
  #  ,
    #--------------------------------
    test.ClassWithMethods=function(){
      self$checkExamplePkg("ClassWithMethods")
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


############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
