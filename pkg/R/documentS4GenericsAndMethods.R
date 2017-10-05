
# vim:set ff=unix expandtab ts=2 sw=2:
documentS4GenericsAndMethods <- function(pkgEnv,pkgDir,GensWithDocMethods){
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
    # Only the generics in the package need their own Rd file
    if (GenHasSrc(genName,pkgDir,pkgEnv)){
      #pe(quote(class(getGeneric(genName))))  
      docObjects <- c(
        docObjects,
        get_docObject(getGeneric(genName),pkgDir) 
      )
    }
  }
  return(docObjects)
}