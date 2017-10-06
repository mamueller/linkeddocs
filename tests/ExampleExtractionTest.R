#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
source("ComponentsTest.R")
ExampleExtractionTest<-R6Class("ExampleExtractionTest",
	inherit=ComponentsTest,
  public=list(
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
      self$assertEqual(res,c(ref1,ref2))
    }
    ,
    #----------------
    test.external_example_lines_for_function=function(SKIP){
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
          do <-  functionDocObject(name=\'f1\',functionObject=f1,pkgDir=\'bla\')
          external_example_lines(do)
          ',
          srcFn)
      ))
      stop('The assertion is missing')



    }
    ,
    #----------------
    test.external_example_lines_for_setClass=function(){
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
      exFuncName  <- 'exFunc1'
      exampleCode <-'
        m <-M(5)
        print(M@t)
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
      M=setClass("M",
        slots=c(t=\'numeric\')
        ##exampleFunctionsFromFiles<< 
        ## %s %s
      )
      ',relPath,exFuncName)
      write(codeText,sprintf('%s/R/source.R',pkgDir))
     
      NAMESPACE_text<-'
      exportClasses(M)
      '
      write(NAMESPACE_text,sprintf('%s/NAMESPACE',pkgDir))

      writeDescriptionFile(Depends="methods",pkgName="blaPkg",pkgDir=pkgDir)

      # find example
      f <- function(pkgDir,results){
        clname <- "M"
        sr <-  findClassSrcRef(results,clname) 
        cdo <- get_docObject( getClass(clname),pkgDir,sr)
        res <- external_example_lines(cdo)
        return(res)
      }
      res <- callWithPackageVars(
        pkgDir,
        workerFunc=f,
        varNamesFromPackageEnv=c("pkgDir","results")
      )
      ref <- '# inst/examples_1.R exFunc1:
      m <-M(5)
      print(M@t)' 
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
