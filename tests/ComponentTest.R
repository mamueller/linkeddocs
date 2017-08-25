#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
ComponentTest<-R6Class("ComponentTest",
	inherit=ExamplePkgTest,
  public=list(
    #----------------
    evalWithExamplePackageLoaded=function(targetPkgName,expr){
      # copy the files 
      self$cp_package_files(targetPkgName)
      devtools::install('pkg',keep_source=TRUE)
      evalWithPackageLoaded(targetPkgName,expr)
    }
    ,
    #----------------
    test.exampleExtractionFromComments=function(){
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          meths <- findMethods(exposedGeneric)
          targetSig <- signature("ExposedClass","numeric")
          sig=meths[[1]]@defined
          meth <- getMethod(exposedGeneric,targetSig)
          do <- get_docObject(meth) 
          l <- do@l
        })
      )
      ref=as.character('
        eci <- new(Class="ExposedClass",1:4)
        exposedGeneric(eci,3)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['examples']],ref))
    }
    ,
    #----------------
    test.exampleFromFunction=function(){
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethodsAndExampleFiles'
        ,
        quote({
          path <- file.path('pkg','inst','examples','examples_1.R')
          source(path,keep.source=TRUE)
          ref <- paste(as.character(path),'func1',collapse=" ")
					extract_function_body_with_comments(func1)
        })
      )
      #pe(quote(res))
      ref=as.character('
        # a comment in the example
        eci <- new(Class="ExposedClass",1:4)
        # another comment
        exposedGeneric(eci,1)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.exampleFunctionFromFile=function(){
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethodsAndExampleFiles'
        ,
        quote({
          path <- file.path('pkg','inst','examples','examples_1.R')
          ref <- paste(as.character(path),'func1',collapse=" ")
          r <- example_lines_from_file(ref)
          r
        })
      )
      ref=as.character('
        # examples from external files
        # inst/examples/examples_1.R func1: 
        # a comment in the example
        eci <- new(Class="ExposedClass",1:4)
        # another comment
        exposedGeneric(eci,1)
        
        # inst/examples/examples_1.R func2: 
        eci <- new(Class="ExposedClass",1:4)
        exposedGeneric(eci,2)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.exampleFunctionFromFiles=function(SKIP){
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethodsAndExampleFiles'
        ,
        quote({
          meths <- findMethods(exposedGeneric)
          targetSig <- signature("ExposedClass","numeric")
          sig=meths[[1]]@defined
          meth <- getMethod(exposedGeneric,targetSig)
          do <- get_docObject(meth) 
          exlines <- Rd_example_lines(do)
        })
      )
      #pe(quote(res))
      pp("res") 
      ref=as.character('
        eci <- new(Class="ExposedClass",1:4)
        exposedGeneric(eci,3)

        # examples from external files
        # inst/examples/example1.R func1: 
        # a comment in the example
        eci <- new(Class="ExposedClass",1:4)
        # another comment
        exposedGeneric(eci,1)
        
        # inst/examples/example1.R func2: 
        eci <- new(Class="ExposedClass",1:4)
        exposedGeneric(eci,2)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
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
