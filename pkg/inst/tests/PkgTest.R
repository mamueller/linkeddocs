#
## vim:set ff=unix expandtab ts=2 sw=2:
require(linkeddocs)
require(R6Unit)
source("ExamplePkgTest.R")
PkgTest<-R6Class("PkgTest",
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
    checkExamplePkg=function(targetPkgName,main=package.skeleton.dx){
      # copy the files 
      self$cp_package_files(targetPkgName)
      # create private library location and make it the first place to look at
      privatePackageLib<-"tmp"
      if (file.exists(privatePackageLib)){unlink(privatePackageLib,recursive=TRUE,force=TRUE)}
      dir.create(privatePackageLib,recursive=TRUE)
      oldp <- .libPaths()
      newp <- append(privatePackageLib,oldp)
      .libPaths(newp)
      on.exit({
        .libPaths(oldp) 
        unlink(privatePackageLib,recursive=TRUE,force=TRUE)
      })

      # create the documentation 
      pkgDir="pkg"
      main(pkgDir)
      
      # perform cran checks
      l<-check(pkgDir,document=FALSE,quiet=TRUE)
      #l<-check(pkgDir,document=FALSE)
      
      #l<-list(errors=c("bla","blub"),warnings=c("foo"),notes=c("bar","foo"))
      #l<-check(pkgDir,document=FALSE,quiet=FALSE)
      self$assertCranResultOk(l)
    }
  )
)
