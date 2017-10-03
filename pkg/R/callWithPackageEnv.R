
# vim:set ff=unix expandtab ts=2 sw=2:
# The purpose of this function is to find
# a way to load the package to document into
# two separate environments
callWithPackageEnv <- function(pkgDir,taskFunc,...){
  results <- objectsAndSrcRefs(pkgDir)
 #
	pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  require(pkgName,character.only=TRUE)
  fqPkgName <- sprintf("package:%s",pkgName)
  pkgEnv <- as.environment(fqPkgName) 
 # pe(quote(getClasses(pkgEnv)))
  res <- taskFunc(pkgEnv,results,...)
  
  return(res)
}
