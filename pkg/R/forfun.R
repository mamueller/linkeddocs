
### For each function in the package, do something.
forfun<- function
### For each object in the package that satisfies the criterion
### checked by subfun, parse source using FUN and return the resulting
### documentation list.
(FUN
### Function to apply to each function in the package.
 ){
  FUN <- FUN
  f <- function(objs,docs,...){
    if(length(objs)==0)return(list())
    objs <- objs[sapply(objs,is.function)]
    L <- list()
    on.exit(cat(sprintf("Parser Function failed on %s\n",N)))
    for(N in names(objs)){
      o <- objs[[N]]
      L[[N]] <- FUN(src=getSource(o),
                    name=N,objs=objs,o=o,docs=docs,doc=docs[[N]],...)
    }
    on.exit()## remove warning message
    L
  }
  class(f) <- c("allfun","function")
  f
### A Parser Function.
}
