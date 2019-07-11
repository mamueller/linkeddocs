
# vim:set ff=unix expandtab ts=2 sw=2:
objectsAndSrcRefs <-  function(pkgDir){
  requireNamespace('pkgload')
  pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  # create but do not load the namespace
  source_env <- pkgload::ns_env(pkgName)
  #source_env <- pkgload:::makeNamespace(pkgName, 1.1)

  print('################################### 1 #####################################')
  print(pkgDir)
  path_r<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- pkgload:::withr_with_collate("C", tools::list_files_with_type(path_r, "code", full.names = TRUE))
  print('################################### 2 #####################################')

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
      print('################################### 3 #####################################')
      print(expr)
      call=as.list(expr)
      print(call)
      call<-standardise_call(call,source_env)
      #res <- eval(expr,source_env)
      results[[j]] <- list()
      results[[j]][['res']] <- res
      results[[j]][['srcref']] <-srcreflist[[i]]
      j=j+1
    }
  }
  return(results)
}
