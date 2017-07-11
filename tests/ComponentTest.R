#!/usr/bin/Rscript 
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
ComponentTest<-R6Class("ComponentTest",
	#inherit=InDirTest,
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
        print(.libPaths())
        res <- self$evalWithExamplePackageLoaded(
        'ClassWithMethods'
        ,
        quote({
          meths <- findMethods(exposedGeneric)
          targetSig <- signature("ExposedClass","numeric")
          sig=meths[[1]]@defined
          meth <- getMethod(exposedGeneric,targetSig)
          pe(quote(meth@defined))
          srcRef <- utils::getSrcref(meth)
          codeText <- as.character(srcRef,useSource=T)
          pp('codeText')
          l <- extract.xxx.chunks(codeText)
          l
        })
      )

      pe(quote(res[['examples']]))
      ref=as.character('
        eci <- new(Class="ExposedClass",1:4)
        exposedGeneric(eci,3)
      ')
      self$assertTrue(CompareTrimmedNonEmptyLines(res[['examples']],ref))
    }
    ,
    #----------------
    test.AWBmodelexample=function(){
        print(.libPaths())
        res <- self$evalWithExamplePackageLoaded(
          'SoilR'
          ,
          quote({
            funcName <-'AWBmodel' 
            func <- get(funcName)
            srcRef <- utils::getSrcref(func)
            codeText <- as.character(srcRef,useSource=T)
            l <- extract.xxx.chunks(codeText)
            fdo <- functionDocObject(name=funcName,l=l,functionObject=func)
            fdo
          })
      )

      pe(quote(class(res)))
      pe(quote(names(res@l)))
      pe(quote(res@l[['examples']]))

      #ref=as.character('
      #  eci <- new(Class="ExposedClass",1:4)
      #  exposedGeneric(eci,3)
      #')
      #self$assertTrue(CompareTrimmedNonEmptyLines(res[['examples']],ref))
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
