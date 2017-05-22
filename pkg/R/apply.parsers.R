
# vim:set ff=unix expandtab ts=2 sw=2:
apply.parsers<- function
### Parse code to r objs, then run all the parsers and return the
### documentation list.
(code,
### Character vector of code lines.
 parsers=default.parsers,
### List of Parser Functions.
 verbose=FALSE,
### Echo names of Parser Functions?
inlinedocs.exampleDir,
### A string pointing to the location where inlinedocs should search for external examples
inlinedocs.exampleTrunk,
### A string used to identify the files containing external examples in the example directory. All file names of external examples have to start with this string
 ...
### Additional arguments to pass to Parser Functions.
 ){
  verbose<-TRUE
  all <- devtools::load_all()
  #fls <- roxygen2:::package_files('..')
  print(all)
  e <- all[['env']]
  objs <- sapply(ls(e),get,e,simplify=FALSE)
  exprs <- parse(text=code,keep.source=TRUE)
  docs <- list()

  ## apply parsers in sequence to code and objs
  if(verbose)cat("Applying parsers:\n")
  for(i in seq_along(parsers)){
    N <- names(parsers[i])
    if(verbose){
      if(is.character(N) && N!=""){
        #cat(" this is parser:",N,"\n",sep="")
      }else cat('.\n')
    }
    p <- parsers[[i]]
    ## This is the argument list that each parser receives:
    L <- p(
	code=code,
	objs=objs,
	docs=docs,
	env=e,
	inlinedocs.exampleDir=inlinedocs.exampleDir,
	inlinedocs.exampleTrunk=inlinedocs.exampleTrunk,
	...
	)
    docs <- combine(docs,L) 
  }
  ## post-process to collapse all character vectors
  for(i in seq_along(docs)){
    for(j in seq_along(docs[[i]])){
      if(names(docs[[i]])[j]!=".s3method")
      docs[[i]][[j]] <- paste(docs[[i]][[j]],collapse="\n")
    }
 }
  if(verbose)cat("\n")

  return(list(docs=docs,env=e,objs=objs,exprs=exprs))
### A list of extracted documentation from code.
}
