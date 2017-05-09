#
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
ExamplePkgTest<-R6Class("ExamplePkgTest",
	inherit=InDirTest,
  public=list(
    targetPkgName=""
    ,
    #----------------
    inDirSetUp=function(){
      targetPkgName=self$targetPkgName
      resourceDirName<-file.path("..","..","test_resources","example_packages")
      pkgDir="pkg"
      cpDir(file.path(resourceDirName,targetPkgName),pkgDir)

      # if necessarry add a default DESCRIPTION file
      if (!file.exists(file.path(pkgDir,"DESCRIPTION"))){ 
        writeDescriptionFile(Depends="methods",pkgName=targetPkgName,pkgDir=pkgDir)
      }
      # create the documentation 
      ## perform cran checks
      #l<-check(pkgDir,document=FALSE,quiet=TRUE)
      #
      ##l<-list(errors=c("bla","blub"),warnings=c("foo"),notes=c("bar","foo"))
      ##l<-check(pkgDir,document=FALSE,quiet=FALSE)
      #self$assertCranResultOk(l)
    }
  )
)
