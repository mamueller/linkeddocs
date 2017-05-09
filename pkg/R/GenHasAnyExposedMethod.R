
# vim:set ff=unix expandtab ts=2 sw=2:
############################################################
GenHasAnyExposedMethod=function
### function used to check if a GenericFunction has any method 

(genName,env,pkgDir){
  
  #decide<-function(MethodDescription){
  #  MethodSignatureHasOnlyExportedClasses(MethodDescription,env,pkgDir)
  #}
  decide<-function(MethodDescription){
    length(MethodDescription)>0
  }
  hasExposedMethod <- any(
      sapply(
        findMethods(genName,where=env)
        ,decide
      )
  )
  hasExposedMethod
}
