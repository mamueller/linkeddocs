checkExamplePkg=function(targetPkgName){
  whereAmI <- getwd()
  # copy the files 
  cp_package_files(targetPkgName)

  # create the documentation 
  pkgDir="pkg"
  package.skeleton.dx_3(pkgDir)
   
  # perform cran checks
  
  l<-devtools::check(pkgDir,document=FALSE,quiet=FALSE,cran=TRUE,check_dir='.')
  assertCranResultOk(l,msg="devtools::check failed")
}
