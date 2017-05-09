#
## vim:set ff=unix expandtab ts=2 sw=2:
require(inlinedocs)
require(R6Unit)
PkgTest<-R6Class("PkgTest",
	inherit=InDirTest,
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
      resourceDirName<-file.path("..","..","test_resources","example_packages")
      pkgDir="pkg"
      cpDir(file.path(resourceDirName,targetPkgName),pkgDir)

      print("############### in inDirSetUp ###########################")
      # if necessarry add a default DESCRIPTION file
      if (!file.exists(file.path(pkgDir,"DESCRIPTION"))){ 
        writeDescriptionFile(Depends="methods",pkgName=targetPkgName,pkgDir=pkgDir)
      }
      # create the documentation 
      package.skeleton.dx(pkgDir)
      
      # perform cran checks
      l<-check(pkgDir,document=FALSE,quiet=TRUE)
      
      #l<-list(errors=c("bla","blub"),warnings=c("foo"),notes=c("bar","foo"))
      #l<-check(pkgDir,document=FALSE,quiet=FALSE)
      self$assertCranResultOk(l)
    }
  )
)
