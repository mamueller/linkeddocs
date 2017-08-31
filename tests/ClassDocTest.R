#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
source("ComponentsTest.R")
ClassDocTest<-R6Class("ClassDocTest",
	inherit=ComponentsTest,
  public=list(
    #----------------
    test.ClassDoc=function(){
      res<- self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          pkgDir <- 'pkg'
          cl <- getClass('ExposedClass')
          cdo <- get_docObject(cl,pkgDir)
          res <- get_xxx_chunks(cdo)
          res
        })
      )
      pe(quote(res))
      ref_title<- 'an Exposed  class'
      ref_description<-c(
        "Since this class is exported in the Namespace file you can inherit from it",
        "but nethertheless the method for \"hiddenGeneric\" with this class as",
        "a signature will not be visible",
        "a signature will not be visible"
      )
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['title']],ref_title))
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['description']],ref_description))
    }
  )
)
############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  source("helpers.R")
  s=get_suite_from_file(get_Rscript_filename())
  s$parallel <- 1 
  tr<-s$run()
  tr$summary()
}
