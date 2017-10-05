
# vim:set ff=unix expandtab ts=2 sw=2:
package.skeleton.dx_3<-function(pkgDir){
  ### call the worker in the environment where 
  ### the package is loaded
  callWithPackageVars(
    pkgDir,
    workerFunc=documentAll,
    varNamesFromPackageEnv=c(
      'pkgEnv',
      'results',
      'pkgDir',
      'manPath',
      'fqPkgName'
    )
  )
}
