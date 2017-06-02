
#
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
PkgTest_3<-R6Class("PkgTest_3",
	#inherit=InDirTest,
	inherit=ExamplePkgTest,
  public=list(
    targetPkgName=""
    ,
    #----------------
    assertCranResultOk=function(l,msg="devtools::check failed"){
      ne<-length(l$errors)
      nw<-length(l$warnings)
      nn<-length(l$notes)
      tn<-ne+nw+nn
      cond<-(ne+nw+nn)>0
      print(cond)
      if(cond){
        print(l)
      }
      self$assertEqual(ne,0)
      self$assertEqual(nw,0)
      self$assertEqual(nn,0)
    }
    ,
    #----------------
    checkExamplePkg=function(targetPkgName){
      whereAmI <- getwd()
      pp('whereAmI')
      # copy the files 
      self$cp_package_files(targetPkgName)

      # create the documentation 
      pkgDir="pkg"
      package.skeleton.dx_3(pkgDir)
       
      # perform cran checks
      #l<-check(pkgDir,document=FALSE,quiet=TRUE,cran=TRUE)
      #l<-check(pkgDir,document=FALSE)
      
      #l<-list(errors=c("bla","blub"),warnings=c("foo"),notes=c("bar","foo"))
      #l<-check(pkgDir,document=FALSE,quiet=FALSE)
      l<-check(pkgDir,document=FALSE,quiet=TRUE,cran=TRUE,check_dir='.')
      self$assertCranResultOk(l)
    }
  )
)
