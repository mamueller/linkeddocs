
# vim:set ff=unix expandtab ts=2 sw=2:
# The purpose of this function is to call worker functions
# that need variables of the package to be documented
# in an environment where this information is present
# It avoids necessaty that every worker function reinitializes those
# variables 
callWithPackageVars <- function(
    pkgDir,     ### The directory of the package to be loaded 
    taskFunc,   ### A function that will be called with the package loaded
                ### and any of the variable in argument varNames
    varNames,   ### The first arguments of the function call
                ### that will be intitialized from this functions local
                ### variables and must be available here.
    ...         ### other arguments the worker function might need
                ### These are initialized from the environment of 
                ### the caller (this is the usual case of any normal  
                ### function call)
  )
  {
  privatePackageLib<-file.path(pkgDir,'..','tmp','lib')
  results <- objectsAndSrcRefs(pkgDir)
  manPath <- file.path(pkgDir,'man')
  if (!file.exists(manPath)){
    dir.create(recursive=TRUE,manPath)
  }else{
    lapply(
       list.files(full.names = TRUE ,manPath,recursive = FALSE,patter='*.Rd'),
      unlink
    )
  }
	
  if (!dir.exists(privatePackageLib)){
    dir.create(privatePackageLib,recursive=TRUE)
  }
  oldLibs <- .libPaths()
  newp <- append(privatePackageLib,oldLibs)
  .libPaths(newp)
  pe(quote(.libPaths()))
 #
	pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
	install.packages(pkgDir,lib=privatePackageLib,repos=NULL,INSTALL_opts="--with-keep.source", type="source")
  require(pkgName,character.only=TRUE)
  fqPkgName <- sprintf("package:%s",pkgName)
  pkgEnv <- as.environment(fqPkgName) 
  on.exit({
    .libPaths(oldLibs) 
    detach(fqPkgName,unload=TRUE,character.only=TRUE) 
    unlink(privatePackageLib,recursive=TRUE,force=TRUE)
    })
 # pe(quote(getClasses(pkgEnv)))
  res <- taskFunc(pkgEnv,results,...)
  
  return(res)
}
