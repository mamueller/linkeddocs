#
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
source('cpDir.R')
source('writeDescriptionFile.R')
source('cp_package_files.R')
requireNamespace("pkgload")
requireNamespace("debugHelpers")
pkgload::load_all('../pkg')

ExamplePkgTest<-R6Class("ExamplePkgTest",
	inherit=InDirTest,
  public=list(
    targetPkgName=""
    ,
    #----------------
    cp_package_files=function(targetPkgName){
      resourceDirName<-file.path("..","..","test_resources","example_packages")
      pkgDir="pkg"
      cpDir(file.path(resourceDirName,targetPkgName),pkgDir)


      # if necessarry add a default DESCRIPTION file
      if (!file.exists(file.path(pkgDir,"DESCRIPTION"))){ 
        writeDescriptionFile(Depends="methods",pkgName=targetPkgName,pkgDir=pkgDir)
      }
    }
  )
)
