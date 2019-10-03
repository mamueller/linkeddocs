#!/usr/bin/Rscript
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
ExamplePkgScriptTest<-R6Class("ExamplePkgScriptTest",
	inherit=InDirScriptTest,
  public=list(
    targetPkgName=""
    ,
    #------------------------
    setUp=function(){
      source('../../cpDir.R')
      source('../../writeDescriptionFile.R')
      source('../../cp_package_files.R')
      requireNamespace("pkgload")
      requireNamespace("debugHelpers")
      pkgload::load_all('../../../pkg')
    }
    ,
    #------------------------
    test.title=function(){
      #source('../../assertCranResultOk.R')
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

      ref_title<- 'an Exposed  class'
    
      stopifnot(CompareTrimmedNonEmptyLines(res,ref_title))

    }
    ,
    #----------------
    test.ClassDocXXX=function(){

      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- get_xxx_chunks(cdo)
          res
      }
      cp_package_files("ClassWithMethods")
      
      res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
      pp('res')


      ref_description<-c(
        "Since this class is exported in the Namespace file you can inherit from it", "but nethertheless the method for \"hiddenGeneric\" with this class as", "a signature will not be visible"
      )
      stopifnot(CompareTrimmedNonEmptyLines(res[['description']],ref_description))
    }
    ,
    
    #----------------
    test.Rd_method_lines=function(){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_method_lines(cdo)
          res
      }
      
      cp_package_files("ClassWithMethods")
      res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
      pp("res")
      ref<-c(
        " Exported methods directly defined for class ExposedClass:\n",
        "  \\describe{",
        "    \\item{[}{\\code{signature(x = \"ExposedClass\", i = \"character\", j = \"missing\", drop = \"missing\")}: ... } \\code{\\link{[,ExposedClass,character,missing,missing-method}}",
        "    \\item{exposedGeneric}{\\code{signature(object = \"ExposedClass\", somethingElse = \"numeric\")}: ... } \\code{\\link{exposedGeneric,ExposedClass,numeric-method}}",
        "\t }"
      ) 
      stopifnot(CompareTrimmedNonEmptyLines(res,ref))
    }
    #,
    #
    ##----------------
    #test.superclass_lines=function(){
    #  pkgDir <- 'pkg'
    #  testfunc <- function(pkgEnv,results,pkgDir){
    #      clname <- 'VirtualParentClass'
    #      sr <-  findClassSrcRef(results,clname)
    #      cl <- getClass(clname)
    #      cdo <- get_docObject(cl,pkgDir,sr)
    #      res <- Rd_superclass_lines(cdo)
    #      res
    #  }
    #  cp_package_files("VirtualClass")
    #  res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
    #  ref="\\code{\\link{VirtualParentParentClass-class}}\\cr"
    #  stopifnot(CompareTrimmedNonEmptyLines(res,ref))
    #}
    #,
    #
    ##----------------
    #test.Rd_constructor_lines_for_virtual_class=function(){
    #  pkgDir <- 'pkg'
    #  testfunc <- function(pkgEnv,results,pkgDir){
    #      clname <- 'VirtualParentParentClass'
    #      sr <-  findClassSrcRef(results,clname)
    #      cl <- getClass(clname)
    #      cdo <- get_docObject(cl,pkgDir,sr)
    #      res <- Rd_constructor_lines(cdo)
    #      res
    #  }
    #  cp_package_files("VirtualClass")
    #  res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
    #  ref="The class is abstract ( \\code{contains \"VIRTUAL\"}).\n           It can therefore not be instanciated directly.\n           Look at non virtual subclasses and their constructors!\n"
    #  #stop('want to see the log')
    #  stopifnot(CompareTrimmedNonEmptyLines(res,ref))
    #}
    #,
    ##----------------
    #test.FindAutoConstructor=function(){
    #  # statements like
    #  # A  <- setC;ass('A',...) 
    #  # should be found as well as
    #  # setClass('A',...) 
    #  # we don not want to use regexp to find the statements     # but rely on R to parse them and then find them
    #  pkgDir <- 'pkg'
    #  testfunc <- function(pkgEnv,results,pkgDir){
    #      clname <- 'RealClass'
    #      sr <-  findClassSrcRef(results,clname)
    #      sr
    #  }
    #  cp_package_files("AutoConstructor")
    #  res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))

    #  stopifnot(getSrcLocation(res)==3)
    #  stopifnot(getSrcFilename(res)=='source.R')
    #}
    #,
    ##----------------
    #test.AutoConstructor_lines=function(){
    #  pkgDir <- 'pkg'
    #  testfunc <- function(pkgEnv,results,pkgDir){
    #      clname <- 'RealClass'
    #      sr <-  findClassSrcRef(results,clname)
    #      cl <- getClass(clname)
    #      cdo <- get_docObject(cl,pkgDir,sr)
    #      res <- Rd_constructor_lines(cdo)
    #      res
    #  }
    #  cp_package_files("AutoConstructor")
    #  res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
    #  pp("res")
    #  ref<-c("\t\\code{\\link{RealClass}}\\cr")
    #  stopifnot(CompareTrimmedNonEmptyLines(res,ref))
    #}
    #,
    ##----------------
    #test.subclass_lines=function(SKIP){
    #  pkgDir <- 'pkg'
    #  testfunc <- function(pkgEnv,results,pkgDir){
    #      clname <- 'ExposedClass'
    #      sr <-  findClassSrcRef(results,clname)
    #      cl <- getClass(clname)
    #      cdo <- get_docObject(cl,pkgDir,sr)
    #      res <- Rd_subclass_lines(cdo)
    #      res
    #  }
    #  cp_package_files("ClassWithMethods")
    #  res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
    #  ref<-c(
    #    "  \\describe{",
    #    "    \\item{[}{\\code{signature(x = \"ExposedClass\", i = \"character\", j = \"missing\", drop = \"missing\")}: ... } \\code{\\link{[,ExposedClass,character,missing,missing-method}}",
    #    "    \\item{exposedGeneric}{\\code{signature(object = \"ExposedClass\", somethingElse = \"numeric\")}: ... } \\code{\\link{exposedGeneric,ExposedClass,numeric-method}}",
    #    "\t }"
    #  ) 
    #  stopifnot(CompareTrimmedNonEmptyLines(res,ref))
    #}
    #,
    ##----------------
    #test.write_Rd_file=function(){
    #  # we only check that the function can be called an produces 
    #  # a .Rd file in the man folder
    #  # the contents of the file are better checked 
    #  # in parts by smaller tests
    #  pkgDir <- 'pkg'
    #  testfunc <- function(pkgEnv,results,pkgDir){
    #      clname <- 'ExposedClass'
    #      sr <-  findClassSrcRef(results,clname)
    #      cl <- getClass(clname)
    #      cdo <- get_docObject(cl,pkgDir,sr)
    #      res <- write_Rd_file(obj=cdo,fn='test.Rd') 
    #  }
    #  cp_package_files("ClassWithMethods")
    #  res <- callWithPackageVars(pkgDir='pkg',workerFunc=testfunc,varNamesFromPackageEnv=c('pkgEnv','results','pkgDir'))
    #  stopifnot(
    #    file.exists('test.Rd') 
    #  )
    #}
  )
)
############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  #s$parallel <- 1 
  tr<-s$run()
  tr$print_summary()
}
