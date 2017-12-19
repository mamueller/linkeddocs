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
    test.SetMethod_lines=function(SKIP){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          genName <- 'BoundFc'
          sr <-  findMethodSrcRef(results,genName)
          #cl <- getClass(genName)
          #cdo <- get_docObject(cl,pkgDir,sr)
          #res <- Rd_constructor_lines(cdo)
          #res
          sr
      }
      self$loadAndInstall("MethodSrcRef")
      res <- callWithPackageVars(pkgDir,workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
      pp("res")
      #ref<-c("\t\\code{\\link{RealClass}}\\cr")
      #self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
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
    ,
    #----------------
    test.details=function(){
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
      
      res <- callWithPackageVars(pkgDir,workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))[['details']]
      print('mm ######################################')
      print(res)
      #stop()
      ref_title<- c('here come a few details','in two lines')
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
