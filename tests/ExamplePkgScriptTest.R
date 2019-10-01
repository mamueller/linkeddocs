#
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
ExamplePkgScriptTest<-R6Class("ExamplePkgScriptTest",
	inherit=InDirScriptTest,
  public=list(
    targetPkgName=""
    ,
    #------------------------
    test.copyPackage=function(){
      source('../../helpers.R')
      source('../../assertCranResultOk.R')
      source('../../cp_package_files.R')
      cp_package_files("ClassWithMethods")
      requireNamespace("pkgload")
      pkgload::load_all('../../../pkg')
      
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_title_lines(cdo)
          res
      }
      cp_package_files("ClassWithMethods")
      
      res <- callWithPackageVars(pkgDir,workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
      #pp('res')

      ref_title<- 'an Exposed  class'
      stopifnot(CompareTrimmedNonEmptyLines(res,ref_title))


      #stopifnot(identical(content,res))
	  }
  )
)
