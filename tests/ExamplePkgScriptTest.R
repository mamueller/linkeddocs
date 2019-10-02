#
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
ExamplePkgScriptTest<-R6Class("ExamplePkgScriptTest",
	inherit=InDirScriptTest,
  public=list(
    targetPkgName=""
    ,
    #------------------------
    test.title=function(){
      #source('../../assertCranResultOk.R')
      source('../../helpers.R')
      source('../../cp_package_files.R')
      requireNamespace("pkgload")
      requireNamespace("debugHelpers")
      pkgload::load_all('../../../pkg')
      #pkgload::load_all('~/debugHelpers/pkg')
      
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_title_lines(cdo)
          res
      }
      cp_package_files("ClassWithMethods")
      
      res <- callWithPackageVars(pkgDir="pkg",workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
      debugHelpers::pp('res')

      ref_title<- 'an Exposed  class'
      stopifnot(CompareTrimmedNonEmptyLines(res,ref_title))
	  }
    ,
    #----------------
    test.classGeneratorFunction=function(){
      source('../../helpers.R')
      source('../../cp_package_files.R')
      requireNamespace("pkgload")
      requireNamespace("debugHelpers")
      pkgload::load_all('../../../pkg')

      testfunc <- function(pkgEnv,results,pkgDir){
          print(results)
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          print("############################ 5 #########################")
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_title_lines(cdo)
          #res <- sr
          res
      }
      cp_package_files("ClassGeneratorFunction")
      
      res <- callWithPackageVars(pkgDir="pkg",workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
      print(pp)
      pp('res')

     # ref_title<- 'an Exposed  class'
    
     # self$assertTrue(CompareTrimmedNonEmptyLines(res,ref_title))

    }
  )
)
