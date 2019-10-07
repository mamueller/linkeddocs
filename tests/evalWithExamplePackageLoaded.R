evalWithExamplePackageLoaded=function(targetPkgName,expr){
  # copy the files 
  cp_package_files(targetPkgName)
  pkgName=pkgload::pkg_name('pkg')
  on.exit({
    unloadNamespace(pkgName) 
  })
  pkgEnv=pkgload::load_all('pkg',export_all=FALSE)$env
  res<-eval(expr,envir=pkgEnv)
}
