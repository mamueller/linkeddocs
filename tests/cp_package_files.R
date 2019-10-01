cp_package_files=function(targetPkgName){
  resourceDirName<-file.path("..","..","test_resources","example_packages")
  pkgDir="pkg"
  cpDir(file.path(resourceDirName,targetPkgName),pkgDir)


  # if necessarry add a default DESCRIPTION file
  if (!file.exists(file.path(pkgDir,"DESCRIPTION"))){ 
    writeDescriptionFile(Depends="methods",pkgName=targetPkgName,pkgDir=pkgDir)
  }
}
