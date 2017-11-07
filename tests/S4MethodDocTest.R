#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
source("ComponentsTest.R")
S4MethodDocTest<-R6Class("S4MethodDocTest",
	inherit=ComponentsTest,
  public=list(
    #----------------
    test.title=function(){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          genName <- 'exposedGeneric'
          allMeths<- findMethods(genName)
          targetSig <- signature("ExposedClass","numeric")
          targetMeth <- allMeths[unlist(lapply(allMeths,function(md){md@target==targetSig}))][[1]]
          mdo <- get_docObject(targetMeth,pkgDir=pkgDir)
          #get_xxx_chunks(mdo)
          Rd_lines(mdo)
      }
      self$loadAndInstall("ClassWithMethods")
      
      res <- callWithPackageVars(pkgDir,workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))[['title']]
      ref_title<- 'exposedGeneric,ExposedClass,numeric-method \n short title'
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref_title))

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
