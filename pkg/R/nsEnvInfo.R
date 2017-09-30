
# vim:set ff=unix expandtab ts=2 sw=2:
nsEnvInfo <- function(pkgDir){
  require(devtools)
  l <- list()
  # before we let 'require' load the package we source it 
  # since we need some srcreferences that R does 
  # not provide 
  # This will make ALL classes and functions 
  # available not only those that are exported
  # we will later rely on the package loading 
  # mechanisms and the "package:pkgName" namespace
  # to determine what is actually visible
  source_env <- devtools:::create_ns_env(pkgDir)
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
  unload(pkgDir)
  pe(quote(getClasses(source_env)))
  #stop('#mmm#')
  #pp('exprs')
  #for (expr in exprs){
  #  pp('expr')
  #  eval(expr,envir=source_env)
  #}
  l[['env']] <- source_env
	l
}
