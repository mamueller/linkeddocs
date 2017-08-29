#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
ExampleExtractionTest<-R6Class("ExampleExtractionTest",
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
    test.methodExampleExtractionFromComments=function(){
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          meths <- findMethods(exposedGeneric)
          targetSig <- signature("ExposedClass","numeric")
          sig=meths[[1]]@defined
          meth <- getMethod(exposedGeneric,targetSig)
          do <- get_docObject(meth,'pkg') 
          l <- get_xxx_chunks(do)
        })
      )
      ref=as.character('
        eci <- new(Class="ExposedClass",times=1:4)
        exposedGeneric(eci,3)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['examples']],ref))
    }
    ,
    #----------------
    test.extract_function_body_with_comments=function(){
        # we only look at one external example function
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethodsAndExampleFiles'
        ,
        quote({
          path <- file.path('pkg','inst','examples','examples_1.R')
          source(path,keep.source=TRUE)
					extract_function_body_with_comments(func1)
        })
      )
      pe(quote(res))
      ref=as.character('
        # a comment in the example
        eci <- new(Class="ExposedClass",times=1:4)
        # another comment
        exposedGeneric(eci,1)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res,ref))
    }
    ,
    #----------------
    test.example_lines_from_file=function(){
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethodsAndExampleFiles'
        ,
        quote({
          path <- file.path('inst','examples','examples_1.R')
          r <- example_lines_from_file(paste(as.character(path),'func1',collapse=" "),'pkg')
          s <- example_lines_from_file(paste(as.character(path),'func2',collapse=" "),'pkg')
          list(r,s)
        })
      )
      refr=as.character('
        # inst/examples/examples_1.R func1: 
        # a comment in the example
        eci <- new(Class="ExposedClass",times=1:4)
        # another comment
        exposedGeneric(eci,1)')
      self$assertTrue(CompareTrimmedNonEmptyLines(res[[1]],refr))
      refs=as.character('
        # inst/examples/examples_1.R func2: 
        eci <- new(Class="ExposedClass",times=1:4)
        exposedGeneric(eci,2)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res[[2]],refs))
    }
    ,
    #----------------
    test.example_references=function(){
      require(stringr)
      # check that the comments refering to external examples are 
      # correctly recognized
      ref1='inst/examples/ex1.R func1'
      ref2='inst/examples/ef2.R func'
      codeText<-unlist(str_split(sprintf( '
      f1=function(x){
        ##exampleFunctionsFromFiles<< 
        ##%s
        ##%s
        x^2
      }
      '
      ,ref1,ref2) ,'\n'))
      res <- example_references(codeText)
      pp('res')
      self$assertEqual(res,c(ref1,ref2))
    }
    ,
    #----------------
    test.external_example_lines_for_function=function(){
      # We create an source code that defines a 
      # function and an external file containing
      # the example referenced in the function
      # 
      # From the newly created function we create a functionDocObject
      # which will be used to find the examplefile and extract the 
      # example code from it.
      require(stringr)
      pkgDir <- 'bla'
      lapply(c('R','inst'),function(subDir){ dir.create(file.path(pkgDir,subDir),recursive=TRUE)})

      relPath<- 'inst/examples_1.R' 
      path<- file.path(pkgDir,relPath)  
      pp('path')
      exFuncName  <- 'exFunc1'
      exampleCode <-'
        x<-2
        f1(x)
      '
      
      exampleWrapperCode<- sprintf(
        '%s <-function(){
          %s
        }',
        exFuncName,
        exampleCode
      )

      write(exampleWrapperCode,path)

      codeText<-sprintf('
      f1=function(x){
        ##exampleFunctionsFromFiles<< 
        ## %s %s
        x^2
      }
      ',relPath,exFuncName)
      srcFn <- sprintf('%s/R/source.R',pkgDir)
      write(codeText,srcFn)
     
      # source code and find example
      res <- eval(parse(text=sprintf('
          source(\'%s\',keep.source=TRUE)
          codeText <- as.character(utils::getSrcref(f1),useSource=T)
          do <-  functionDocObject(name=\'f1\',l=extract.xxx.chunks(codeText),functionObject=f1,src=codeText,pkgDir=\'bla\')
          external_example_lines(do)
          ',
          srcFn)
      ))
      pp('res')



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
