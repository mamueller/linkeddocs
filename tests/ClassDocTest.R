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
    test.ClassDocXXX=function(){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- get_xxx_chunks(cdo)
          res
      }
      self$loadAndInstall("ClassWithMethods")
      
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)

      ref_title<- 'an Exposed  class'
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['title']],ref_title))

      ref_description<-c(
        "Since this class is exported in the Namespace file you can inherit from it", "but nethertheless the method for \"hiddenGeneric\" with this class as", "a signature will not be visible"
      )
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['description']],ref_description))
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
      
      self$loadAndInstall("ClassWithMethods")
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)
      pp('res')
      ref<-c(
        "  \\describe{",
        "    \\item{[}{\\code{signature(x = \"ExposedClass\", i = \"character\", j = \"missing\", drop = \"missing\")}: ... } \\code{\\link{[,ExposedClass,character,missing,missing-method}}",
        "    \\item{exposedGeneric}{\\code{signature(object = \"ExposedClass\", somethingElse = \"numeric\")}: ... } \\code{\\link{exposedGeneric,ExposedClass,numeric-method}}",
        "\t }"
      ) 
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    
    #----------------
    test.superclass_lines=function(){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'VirtualParentClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_superclass_lines(cdo)
          res
      }
      self$loadAndInstall("VirtualClass")
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)
      pp('res')
      ref="\\code{\\link{VirtualParentParentClass-class}}\\cr"
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    
    #----------------
    test.Rd_constructor_lines_for_virtual_class=function(){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'VirtualParentParentClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_constructor_lines(cdo)
          res
      }
      self$loadAndInstall("VirtualClass")
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)
      pp('res')
      ref="The class is abstract ( \\code{contains \"VIRTUAL\"}).\n           It can therefore not be instanciated directly.\n           Look at non virtual subclasses and their constructors!\n"
      #stop('want to see the log')
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.FindAutoConstructor=function(){
      # statements like
      # A  <- setC;ass('A',...) 
      # should be found as well as
      # setClass('A',...) 
      # we don not want to use regexp to find the statements     # but rely on R to parse them and then find them
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'RealClass'
          sr <-  findClassSrcRef(results,clname)
          sr
      }
      self$loadAndInstall("AutoConstructor")
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)

      self$assertTrue(getSrcLocation(res)==3)
      self$assertTrue(getSrcFilename(res)=='source.R')
    }
    ,
    #----------------
    test.AutoConstructor_lines=function(){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'RealClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_constructor_lines(cdo)
          res
      }
      self$loadAndInstall("AutoConstructor")
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)
      pp('res')
      ref<-c("\t\\code{\\link{RealClass}}\\cr",
      " Please also look at constructors of non virtual subclasses ") 
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.subclass_lines=function(SKIP){
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- Rd_subclass_lines(cdo)
          res
      }
      self$loadAndInstall("ClassWithMethods")
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)
      pp('res')
      ref<-c(
        "  \\describe{",
        "    \\item{[}{\\code{signature(x = \"ExposedClass\", i = \"character\", j = \"missing\", drop = \"missing\")}: ... } \\code{\\link{[,ExposedClass,character,missing,missing-method}}",
        "    \\item{exposedGeneric}{\\code{signature(object = \"ExposedClass\", somethingElse = \"numeric\")}: ... } \\code{\\link{exposedGeneric,ExposedClass,numeric-method}}",
        "\t }"
      ) 
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.write_Rd_file=function(){
      # we only check that the function can be called an produces 
      # a .Rd file in the man folder
      # the contents of the file are better checked 
      # in parts by smaller tests
      pkgDir <- 'pkg'
      testfunc <- function(pkgEnv,results,pkgDir){
          clname <- 'ExposedClass'
          sr <-  findClassSrcRef(results,clname)
          cl <- getClass(clname)
          cdo <- get_docObject(cl,pkgDir,sr)
          res <- write_Rd_file(obj=cdo,fn='test.Rd') 
      }
      self$loadAndInstall("ClassWithMethods")
      res <- callWithPackageEnv(pkgDir,testfunc,pkgDir)
      self$assertTrue(
        file.exists('test.Rd') 
      )
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
