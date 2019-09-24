
# vim:set ff=unix expandtab ts=2 sw=2:
objectsAndSrcRefs <-  function(pkgDir){
  # the purpose of this function is to combine 
  # the sourcereferences with the actual objects
  # This is done in two steps 
  # 1.) creating the environment wiht pkg_load::load_all
  # 2.) parsing the files into calls (but not evaluating them) 
  #     and finding the corresponding 
  #     objects in the environment creating in step 1.).
  requireNamespace('pkgload')
  pkgName<-as.character(read.dcf(file=file.path(pkgDir,'DESCRIPTION'),fields='Package'))
  # create but do not load the namespace
  #source_env <- pkgload::ns_env(pkgName)
  #source_env <- pkgload:::makeNamespace(pkgName, 1.1)

  path_r<-normalizePath(file.path(pkgDir,'R'))
  codeFiles <- pkgload:::withr_with_collate("C", tools::list_files_with_type(path_r, "code", full.names = TRUE))

  results <- list()
  j=1
  #env=pkgload::load_all(pkgDir,export_all=FALSE)
  env=pkgload::load_all(pkgDir)
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
      print('################################### 3 #####################################')
      print(call)
      #print(srcreflist[i])
      #print(is.call(call))
      call<-standardise_call(call,source_env)
      name <- as.character(call[[1]])
      if (length(name) > 1) return(NULL)
      parser <- find_parser(name)
      if (is.null(parser)) return(NULL)
      # we have to change the parsers not to output the s3 object
      res <- parser(call, env)#, block)
      
      #stop('')
      results[[j]] <- list()
      results[[j]][['res']] <- res
      results[[j]][['srcref']] <-srcreflist[[i]]
      j=j+1
    }
    print('################################### 4 #####################################')
    print(results)
  }
  return(results)
}
