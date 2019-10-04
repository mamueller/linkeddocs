
# vim:set ff=unix expandtab ts=2 sw=2:
documentS4GenericsAndMethods <- function(pkgEnv,results,pkgDir,GensWithDocMethods){
  GenericsDefinedByPackage=getGenerics(pkgEnv)
  docObjects <- c()
  for (genName in GensWithDocMethods){
    meths<- findMethods(genName,where=pkgEnv)
    docObjects <- c(
      docObjects,
      lapply(
        meths,
        function(m){
          get_docObject(m,pkgDir=pkgDir)
        }
      )
    )
    # not all the generics our package defines mehtods for are also defined by the 
    # package. Some like [, [[, $ have been there before.
    # Only the generics defined in the package need their own Rd file
    #if (GenHasSrc(genName,results,pkgDir,pkgEnv)){
    if (genName %in% GenericsDefinedByPackage){
      pp('genName')
      sr <-  findGenericSrcRef(results,genName) 
      ndo <- get_docObject(getGeneric(genName),pkgDir,sr) 
      docObjects <- c(docObjects,ndo )
    }
  }
  return(docObjects)
}
