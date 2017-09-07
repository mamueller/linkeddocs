
extra.code.docs <- function # Extract documentation from code chunks
### Parse R code to extract inline documentation from comments around
### each function. These are not able to be retreived simply by
### looking at the "source" attribute. This is a Parser Function that
### can be used in the parser list of package.skeleton.dx(). TODO:
### Modularize this into separate Parsers Functions for S4 classes,
### prefixes, ##<<blocks, etc. Right now it is not very clean!
(code,
### Code lines in a character vector containing multiple R objects to
### parse for documentation.
 objs,
### The objects defined in the code.
env, # the environment 
 ...
### ignored
 ){
  parsed <- extract.file.parse(code,env)
  doc.names <- names(objs)
  res <- sapply(doc.names,extract.docs,parsed=parsed,objs=objs,simplify=FALSE)
  all.done <- FALSE
  while ( !all.done ){
#print("inherit.docs called by extra.code.docs") 
    res1 <- sapply(doc.names,inherit.docs,parsed=parsed,res=res,simplify=FALSE)
    all.done <- identical(res1,res)
    res <- res1
  }
  ## now strip out any generics (which have value NULL in res):
  res.not.null <- sapply(res,function(x){!is.null(x)})
  if ( 0 < length(res.not.null) && length(res.not.null) < length(res) ){
    res <- res[res.not.null]
  }
  res
### named list of lists, one for each object to document.
}
