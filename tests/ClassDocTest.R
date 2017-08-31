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
          pkgR<-normalizePath(file.path(pkgDir,'R'))
          codeFiles <- list.files(pkgR,full.names=TRUE)
          code<- ''
          for (fn in codeFiles){code <- append(code,readLines(fn))}
          #
          cl <- getClass('ExposedClass')
          cdo <- get_docObject(cl,pkgDir)
          #res <- list()
          #res['cdo'] <- cdo
          cdo
        })
      )
      pp('res')
      stop()
      #self$assertTrue(CompareTrimmedNonEmptyLines(res[['examples']],ref))
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
