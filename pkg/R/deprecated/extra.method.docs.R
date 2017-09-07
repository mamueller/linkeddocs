
extra.method.docs <- function 
### can be used in the parser list of package.skeleton.dx(). TODO:
(code,
### Code lines in a character vector containing multiple R objects to
### parse for documentation.
objs,
### The objects defined in the code.
env, 
### The environment they inhibit (needed to pass on)
inlinedocs.exampleDir,
### A string pointing to the location where inlinedocs should search for external examples
inlinedocs.exampleTrunk,
### A string used to identify the files containing external examples in the example directory. All file names of external examples have to start with this string
...
### ignored
 ){
  doc.names <- names(objs)
  parsed <- extract.file.parse(code,env)
  res=list()
  for ( nn in names(parsed) ){
    dL=parsed[[nn]]
    if ( dL@created == "setMethod" ){
      S4Method.docs <- extract.docs.setMethod(dL,env,inlinedocs.exampleDir,inlinedocs.exampleTrunk)
      docname <- dL@name
      if ( is.null(res[[docname]]) ){
        res[[docname]] <- S4Method.docs
        doc.names <- c(doc.names,docname)
      } else {
        stop(nn," appears as both S4 method and some other definition")
      }
    }
  }
  all.done <- FALSE
  while ( !all.done ){
    res1 <- sapply(doc.names,inherit.docs,parsed=parsed,res=res,simplify=FALSE)
    
    all.done <- identical(res1,res)
    res <- res1
  }
  res
### named list of lists, one for each object to document.
}
