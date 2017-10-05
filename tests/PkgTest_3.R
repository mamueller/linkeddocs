
#
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
CranResultOk=function(l){
  ne<-length(l$errors)
  nw<-length(l$warnings)
  nn<-length(l$notes)
  tn<-ne+nw+nn
  cond<-(ne+nw+nn)==0
  if(!cond){
    print(l)
  }
  cond
}
PkgTest_3<-R6Class("PkgTest_3",
	#inherit=InDirTest,
	inherit=ExamplePkgTest,
  public=list(
    targetPkgName=""
    ,
    #----------------
    assertCranResultOk=function(l,msg="devtools::check failed"){
      self$assertTrue(CranResultOk(l))
    }
    ,
    #----------------
    checkExamplePkg=function(targetPkgName){
      whereAmI <- getwd()
      # copy the files 
      self$cp_package_files(targetPkgName)

      # create the documentation 
      pkgDir="pkg"
      package.skeleton.dx_3(pkgDir)
       
      # perform cran checks
      
      # first run the checks quietly to keep the check out of the logfile
      l<-check(pkgDir,document=FALSE,quiet=TRUE,cran=TRUE,check_dir='.')
      # if there is a problem repeat with all the output
      if (!(CranResultOk(l))){
        l<-check(pkgDir,document=FALSE,quiet=FALSE,cran=TRUE,check_dir='.')
      }
      self$assertCranResultOk(l,msg="devtools::check failed")
    }
  )
)
