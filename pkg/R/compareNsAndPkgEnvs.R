
# vim:set ff=unix expandtab ts=2 sw=2:
# The purpose of this function is to find
# a way to load the package to document into
# two separate environments
compareNsAndPkgEnvs<- function(pkgDir,taskFunc){
  require(devtools)
  pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  # create but do not load the namespace
  source_env <- devtools::: makeNamespace(pkgName, 1.1)
  #pe(quote(loadedNamespaces()))
  #pp('source_env')

  pkgR<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- list.files(pkgR,full.names=TRUE)
  #exprs <- c()
  results <- list()
  j=1
  for (fn in codeFiles){
    print(fn)
    lines <- readLines(fn)
    sf <- srcfile(fn)
    exprs <- parse(text=lines,srcfile=sf,keep.source=TRUE)
    srcreflist <- attr(exprs,'srcref')
    pd <- getParseData(exprs)
    n <- length(exprs)
    for (i in seq_len(n)){
      print(j)
      expr <- exprs[[i]]
      res <- eval(expr,source_env)
      pe(quote(getSrcref(expr)))
      results[[j]] <- list()
      results[[j]][['res']] <- res
      results[[j]][['srcRef']] <-srcreflist[[i]]
      j=j+1
    }
  }
 # pe(quote(getClasses(source_env)))
 # #unload(pkgDir)
 # unloadNamespace('pkgName')
 # pe(quote(getClasses(source_env)))
  require(pkgName,character.only=TRUE)
  fqPkgName <- sprintf("package:%s",pkgName)
  pkgEnv <- as.environment(fqPkgName) 
 # pe(quote(getClasses(pkgEnv)))
  taskFunc(pkgEnv,source_env,results)
  
  return(results)
}
