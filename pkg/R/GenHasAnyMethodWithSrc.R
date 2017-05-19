
# vim:set ff=unix expandtab ts=2 sw=2:
############################################################
GenHasAnyMethodWithSrc=function
### function to check if we have a src reference for any of the methods of this generic
### This helps to decide how the *-methods.Rd file should look like for this generic
(genName,pkgDir){
  methDefs <- findMethods(genName)
  any(sapply(
    methDefs,
    MethodHasSrc,
    pkgDir))
}
