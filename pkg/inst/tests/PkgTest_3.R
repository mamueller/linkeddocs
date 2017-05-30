#
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("PkgTest.R")
PkgTest_3<-R6Class("PkgTest_3",
	inherit=PkgTest,
  public=list(
    #----------------
    checkExamplePkg=function(targetPkgName,main=package.skeleton.dx_3){
      super$checkExamplePkg(targetPkgName,main)
    }
  )
)
