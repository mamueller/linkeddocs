
# vim:set ff=unix expandtab ts=2 sw=2:
# The purpose of this function is to call worker functions
# that need variables of the package to be documented
# in an environment where this information is present
# and the package is loaded
# It avoids the necessity that every worker function reinitializes those
# variables 
callWithPackageVars <- function(
    pkgDir,     ### The directory of the package to be loaded 
    workerFunc, ### A function that will be called with the package loaded
                ### and any of the variable in argument varNamesFromPackageEnv
    varNamesFromPackageEnv,   ### The first arguments of the function call
                ### that will be intitialized from this functions local
                ### variables and must be available here.
    ...         ### other arguments the worker function might need
                ### These are initialized from the environment of 
                ### the caller of this function 
                ###(which is the usual case of any normal  
                ### function call)
  )
  {
  #################################################333
  ### fill the local environment with commonly used variables
  privatePackageLib<-file.path(pkgDir,'..','tmp','lib')
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
 #
	pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
	install.packages(pkgDir,lib=privatePackageLib,repos=NULL,INSTALL_opts="--with-keep.source", type="source")
  requireNamespace(pkgName,character.only=TRUE)
  fqPkgName <- sprintf("package:%s",pkgName)
  #pkgEnv <- as.environment(fqPkgName) 
  on.exit({
    .libPaths(oldLibs) 
    print(search())
    unloadNamespace(fqPkgName) 
    unlink(privatePackageLib,recursive=TRUE,force=TRUE)
    })
  
  results <- objectsAndSrcRefs(pkgDir)
  ##############################################
  # create the function call
  # gather the values for varNamesFromPackageEnv from the local environment
	e <- environment()
	valuesProvidedByThisFunction <- as.list(e)[varNamesFromPackageEnv]
 
  # transform the additional arguments into a list
  valuesProvidedByCaller <- list(...)

  # create the complete parameterlist for the function call
	values <- c( valuesProvidedByThisFunction, valuesProvidedByCaller) 

  #create the actual call
	funcCall <- as.call(append(list(workerFunc),values))
	res <- eval(funcCall)
	
  res
}
