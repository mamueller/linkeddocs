
# vim:set ff=unix expandtab ts=2 sw=2:
# The purpose of this function is to find
# a way to load the package to document into
# two separate environments
compareNsAndPkgEnvs<- function(pkgDir){
  require(devtools)
  #source_env <- devtools:::create_ns_env(pkgDir)
  source_env <- devtools::: makeNamespace(pkgName, 1.1)
  pe(quote(loadedNamespaces()))
  #pp('source_env')
  pkgR<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- list.files(pkgR,full.names=TRUE)
  #exprs <- c()
  results <- list()
  j=1
  for (fn in codeFiles){
    lines <- readLines(fn)
    sf <- srcfile(fn)
    exprs <- parse(text=lines,srcfile=sf,keep.source=TRUE)
    #print(sprintf('getParseData=%s',getParseData(exprs)))
    n <- length(exprs)
    for (i in seq_len(n)){
      expr <- exprs[[i]]
      res <- eval(expr,source_env)
      results[[j]] <- list()
      results[[j]][['res']] <- res
      results[[j]][['srcRef']] <- getParseData(expr)
      j=j+1
    }
  }
  pe(quote(getClasses(source_env)))
  #unload(pkgDir)
  unloadNamespace('pkgName')
  pe(quote(getClasses(source_env)))
  pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  require(pkgName,character.only=TRUE)
  fqPkgName <- sprintf("package:%s",pkgName)
  pkgEnv <- as.environment(fqPkgName) 
  pe(quote(getClasses(pkgEnv)))
}
