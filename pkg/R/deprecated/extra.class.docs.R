
extra.class.docs <- function # Extract documentation from code chunks
### Parse R code to extract inline documentation from comments around
### each class 
### looking at the "source" attribute. This is a Parser Function that
### can be used in the parser list of package.skeleton.dx(). TODO:
(code,
### Code lines in a character vector containing multiple R objects to
### parse for documentation.
objs,
### The objects defined in the code.
env, 
### The environment they inhibit (needed to pass on)
...
### ignored
 ){
  doc.names <- names(objs)
  parsed <- extract.file.parse(code,env)
  res=list()
  for ( nn in names(parsed) ){
    if ( parsed[[nn]]@created == "setClass" ){
      S4class.docs <- extract.docs.setClass(parsed[[nn]])
      docname <- paste(nn,"class",sep="-")
      if ( is.null(res[[docname]]) ){
        res[[docname]] <- S4class.docs
        doc.names <- c(doc.names,docname)
      } else {
        stop(nn," appears as both S4 class and some other definition")
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
