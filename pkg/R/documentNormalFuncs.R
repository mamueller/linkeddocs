
# vim:set ff=unix expandtab ts=2 sw=2:
documentNormalFuncs <- function(
	pkgEnv, 
  pkgDir, 
  namesOfNormalFuncs
){
  res <- lapply(
    namesOfNormalFuncs, 
    function(funcName) {
      obj<-get(funcName,envir=pkgEnv)
      functionDocObject(
        name=funcName,
        functionObject=obj,
        pkgDir=pkgDir
      )
    }
  )
  res 
}
