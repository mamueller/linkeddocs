
# vim:set ff=unix expandtab ts=2 sw=2:
objectsAndSrcRefsForClassesAndMethods <-  function(pkgDir,pkgEnv){
  # the purpose of this function is to combine 
  # the sourcereferences with the actual objects
  # This is done as follows
  # 1.) parsing the files into calls (but not evaluating them) 
  # 2.) finding the corresponding objects in the pkgEnv creating in step 1.).
  requireNamespace('pkgload')
  pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  # create but do not load the namespace

  path_r<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- pkgload:::withr_with_collate("C", tools::list_files_with_type(path_r, "code", full.names = TRUE))

  results <- list()
  j=1
  for (fn in codeFiles){
    lines <- readLines(fn)
    sf <- srcfile(fn)
    exprs <- parse(text=lines,srcfile=sf,keep.source=TRUE)
    calls <- as.list(exprs)
    srcreflist <- attr(exprs,'srcref')
    n <- length(exprs)
    for (i in seq_len(n)){
      expr <- exprs[[i]]
      call <- calls[[i]]
      call<-standardise_call(call,pkgEnv)
      name <- as.character(call[[1]])
      if (length(name) > 1) next
      parser <- find_parser(name)
      if (is.null(parser)) next
      # we have to change the parsers not to output the s3 object
      res <- parser(call, pkgEnv)#, block)
      
      #stop('')
      results[[j]] <- list()
      results[[j]][['res']] <- res
      results[[j]][['srcref']] <-srcreflist[[i]]
      j=j+1
    }
  }
  return(results)
}
