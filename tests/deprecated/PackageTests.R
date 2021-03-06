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

############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  s$parallel <- 1 
  tr<-s$run()
  tr$summary()
}
