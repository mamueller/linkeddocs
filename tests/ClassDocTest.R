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
      res<- self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          pkgDir <- 'pkg'
          cl <- getClass('ExposedClass')
          e=new.env()
          cdo <- get_docObject(cl,pkgDir,e)
          res <- get_xxx_chunks(cdo)
          res
        })
      )
      ref_title<- 'an Exposed  class'
      ref_description<-c(
        "Since this class is exported in the Namespace file you can inherit from it",
        "but nethertheless the method for \"hiddenGeneric\" with this class as",
        "a signature will not be visible"
      )
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['title']],ref_title))
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['description']],ref_description))
    }
    ,
    
    #----------------
    test.ClassDocRd_method_lines=function(){
      res<- self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          pkgDir <- 'pkg'
          cl <- getClass('ExposedClass')
          e=new.env()
          cdo <- get_docObject(cl,pkgDir,e)
          res <- Rd_method_lines(cdo)
          res
        })
      )
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
    test.ClassDocRd_superclass_lines=function(){
      res<- self$evalWithExamplePackageLoaded(
        'VirtualClass'
        ,
        quote({
          pkgDir <- 'pkg'
          cl <- getClass('VirtualParentClass')
          e=new.env()
          cdo <- get_docObject(cl,pkgDir,e)
          res <- Rd_superclass_lines(cdo)
          res
        })
      )
      pp('res')
      ref="\\code{\\link{VirtualParentParentClass-class}}\\cr"
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    
    #----------------
    test.ClassDocRd_constructor_lines_for_virtual_class=function(){
      res<- self$evalWithExamplePackageLoaded(
        'VirtualClass'
        ,
        quote({
          pkgDir <- 'pkg'
          cl <- getClass('VirtualParentParentClass')
          e=new.env()
    			cdo <- get_docObject(cl,pkgDir,e)
          res <- Rd_constructor_lines(cdo)
          res
        })
      )
      pp('res')
      ref="The class is abstract ( \\code{contains \"VIRTUAL\"}).\n           It can therefore not be instanciated directly.\n           Look at non virtual subclasses and their constructors!\n"
      #stop('want to see the log')
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.ClassDocRd_AutoConstructor_lines=function(){
      res<- self$evalWithExamplePackageLoaded(
        'AutoConstructor'
        ,
        quote({
          pkgDir <- 'pkg'
          cl <- getClass('RealClass')
          e=new.env()
        	cdo <- get_docObject(cl,pkgDir,e)
          res <- Rd_constructor_lines(cdo)
          res
        })
      )
      pp('res')
      ref<-c("\t\\code{\\link{RealClass}}\\cr",
      " Please also look at constructors of non virtual subclasses ") 
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.ClassDocRd_subclass_lines=function(SKIP){
      res<- self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          pkgDir <- 'pkg'
          cl <- getClass('ExposedClass')
          e=new.env()
    			cdo <- get_docObject(cl,pkgDir,e)
          res <- Rd_subclass_lines(cdo)
          res
        })
      )
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
    test.ClassDoc_write_Rd_file=function(){
      # we only check that the function can be called an produces 
      # a .Rd file in the man folder
      # the contents of the file are better checked 
      # in parts by smaller tests
      
      expr<- quote({
        pkgDir <- "pkg"
        cl <- getClass('ExposedClass')
        e=new.env()
        cdo <- get_docObject(cl,pkgDir,e)
        res <- write_Rd_file(obj=cdo,fn='test.Rd') 
      })
      res<- self$evalWithExamplePackageLoaded(
        'ClassWithMethods',
         expr
      )
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
