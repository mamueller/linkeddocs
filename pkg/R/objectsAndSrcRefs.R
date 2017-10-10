
# vim:set ff=unix expandtab ts=2 sw=2:
objectsAndSrcRefs <-  function(pkgDir){
  require(devtools)
  pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  # create but do not load the namespace
  source_env <- devtools::: makeNamespace(pkgName, 1.1)

  pkgR<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- list.files(pkgR,full.names=TRUE)
  results <- list()
  j=1
  for (fn in codeFiles){
    lines <- readLines(fn)
    sf <- srcfile(fn)
    exprs <- parse(text=lines,srcfile=sf,keep.source=TRUE)
    srcreflist <- attr(exprs,'srcref')
    n <- length(exprs)
    for (i in seq_len(n)){
      expr <- exprs[[i]]
      res <- eval(expr,source_env)
      results[[j]] <- list()
      results[[j]][['res']] <- res
      results[[j]][['srcref']] <-srcreflist[[i]]
      j=j+1
    }
  }
  return(results)
}
