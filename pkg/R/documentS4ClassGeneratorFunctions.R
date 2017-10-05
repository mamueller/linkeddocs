
# vim:set ff=unix expandtab ts=2 sw=2:
documentS4ClassGeneratorFunctions <- function(pkgEnv,pkgDir,autoConstructorNames){
  res <-  lapply(
     autoConstructorNames,
     function(funcName){
       func <- get(funcName)
       obj <- func@.Data
       fdo=autoConstructorDocObject(
         name=funcName,
         functionObject=obj,
         pkgDir=pkgDir
       )
     }
  )
  return(res)
}
