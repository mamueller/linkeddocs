#!/usr/bin/Rscript
## vim:set ff=unix expandtab ts=2 sw=2:
source("helpers.R")
require(devtools,quiet=TRUE)
FileHelpersTest<-R6Class("FileHelpersTest",
	inherit=InDirTest,
	public=list(
		test.cpDir=function(){
      
      #create a nested testdir
      pkgDir <- "test"
      RDir<-"R"
      manDir<-"man"
      manManDir<-file.path("man","manMan")
      dir.create(file.path(pkgDir,RDir),recursive=TRUE)
      dir.create(file.path(pkgDir,manDir),recursive=TRUE)
      dir.create(file.path(pkgDir,manManDir),manManDir,recursive=TRUE)
      testFileName <- "source.R"
      file.create(file.path(pkgDir,RDir,testFileName))

      # copy the whole tree
      back<-paste0(pkgDir,"_bak")
      cpDir(pkgDir,back)
      #
      ##check if everything is there
      self$assertTrue(dir.exists(file.path(back,RDir)))
      self$assertTrue(dir.exists(file.path(back,manDir)))
      self$assertTrue(dir.exists(file.path(back,manManDir)))
      self$assertTrue(file.exists(file.path(back,RDir,testFileName)))
		}
    ,
    #--------------------------------
		test.write_and_read=function(){
      content="swimming"
      fn="myPersonalTestFile"
			write(content,file=fn)
      res=readLines(fn)[[1]]  
		}
  )
)
############################################ 

if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  tr<-s$run()
  tr$summary()
}
